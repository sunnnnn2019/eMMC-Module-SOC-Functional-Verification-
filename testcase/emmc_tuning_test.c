//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2007-2012 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      RCS Information
//
//      RCS Filename        : $RCSfile$
//
//      Checked In          : $Date$
//
//      Revision            : $Revision: 73865 $
//
//      Release Information : r4p1-00rel0
//-----------------------------------------------------------------------------
//


#include <stdarg.h>
#include <string.h>
#include <stdio.h>

/* *********************************************************************************************
 * IMPLEMENTATION DEPENDANT PARAMETERS
 * ********************************************************************************************/
#define HEAP_BASE               0x00A0000   /* Must point out to a region RW enabled
                                                and having at least 0x90 * number_of_cpus free */

#define IDU_BASE                0x80000

#define PERIPH_BASE    0x10000000
#define CORTEXA9_SCU_BASE       PERIPH_BASE + 0x0000 // SCU registers

#define SCU_CPU_STATES_REG      (unsigned*)(CORTEXA9_SCU_BASE + 0x08)
#define SCU_CONTROL_REG         (unsigned*)(CORTEXA9_SCU_BASE + 0x00)
#define SCU_INVALIDATE_ALL_REG  (unsigned*)(CORTEXA9_SCU_BASE + 0x0C)
#define SCU_EN                  0x1 // Enable the SCU

#define GIC_DIST_BASE  PERIPH_BASE + 0x1000 // Distributor

#define INT_PERIPH_ID4        0xFD0
#define INT_PERIPH_ID5        0xFD4
#define INT_PERIPH_ID6        0xFD8
#define INT_PERIPH_ID7        0xFDC
#define INT_PERIPH_ID0        0xFE0
#define INT_PERIPH_ID1        0xFE4
#define INT_PERIPH_ID2        0xFE8
#define INT_PERIPH_ID3        0xFEC
#define INT_PRIMECELL_ID0     0xFF0
#define INT_PRIMECELL_ID1     0xFF4
#define INT_PRIMECELL_ID2     0xFF8
#define INT_PRIMECELL_ID3     0xFFC


#define FREE 0x1
#define TAKEN 0x0



#ifndef __stdlib_h
  typedef unsigned int size_t;
#endif




typedef volatile int sem_t;
typedef volatile int tmp_sem_t;

typedef struct BARRIER {
  sem_t        lock;
  volatile int release;
  volatile int count;
} Barrier;



Barrier wbwas_barrier;
volatile int how_many_of_us;
sem_t printf_mutex;



typedef struct block_manager_ {
  struct block_manager_ *prev;
  struct block_manager_ *next;
  void *data;
  int isfree;
  size_t size;
} block_manager;



struct heap_manager {
  void * bottom_of_heap;
  int heap_size;
  block_manager *first_block;
  sem_t lock;
};




struct heap_manager hmngr;



void a_printf(const char *str, ...);

void enter_test0();
void enter_test1();
void enter_test2();
void enter_test3();



/*************************************************************************
  Semaphore / Memory barriers related functions
  ***********************************************************************/
#pragma arm

void get_semaphore(sem_t *semaphore)
{
  tmp_sem_t tmp;
  tmp_sem_t taken = TAKEN;
  tmp_sem_t free  = FREE;
  tmp_sem_t success = 0;
  __asm {
get_lock_loop:
    LDREX   tmp, [semaphore];
    CMP     tmp, free;
    BNE     get_lock_loop;
    STREX   tmp, taken, [semaphore];
    CMP     tmp, success;
    BNE     get_lock_loop;
  };
}



void release_semaphore(sem_t *semaphore)
{
  int tmp;
  int free = FREE;

  do {
    __asm {
      LDREX   tmp, [semaphore];
    };

    if (tmp == TAKEN) {
      __asm {
	STREX   tmp, free, [semaphore];
      };
    }
  } while (tmp != 0);
}


void atomic_inc_barrier(Barrier *barrier) {
  get_semaphore(&(*barrier).lock);
  (*barrier).count += 1;
  release_semaphore(&(*barrier).lock);
}

void atomic_dec_barrier(Barrier *barrier) {
  get_semaphore(&(*barrier).lock);
  (*barrier).count -= 1;
  release_semaphore (&(*barrier).lock);
}


void memory_barrier(Barrier * barrier, int numberOfCPU)
{
  (*barrier).release = 0;
  atomic_inc_barrier(barrier);
  /* Barrier entry point: all processors will wait afterwards */
  while (((*barrier).count != (numberOfCPU))  /* Wait for all processors to come here */
	 && ((*barrier).release == 0))      /* Or for one processor to have crossed the barrier */
    ;
  /* Now that all processors have crossed the barrier,
     wait until all processors have crossed the barrier */
  (*barrier).release = 1;

  atomic_dec_barrier(barrier);

  /* Now wait until all processors have crossed the release line */

  while (((*barrier).count != 0)
    && ((*barrier).release == 1))
    ;
}



int get_cpuid() {
  int CPUID;
  __asm {
      MRC	p15, 0, CPUID, c0, c0, 5
      }
  CPUID = CPUID & 0xf;
  return (CPUID);
}




/*************************************************************************
  SCU
  ***********************************************************************/

/* Get the number of powered on CPUs in MPCore */
int get_number_of_cpu(void) {
  unsigned cpu_status;
  int num_cpu = 0;
  int i;
  volatile unsigned int *cpu_status_ptr = SCU_CPU_STATES_REG;
  cpu_status = *cpu_status_ptr;
  for(i = 0; i <4; i++)
  	num_cpu += !((cpu_status >> i * 8) & 0x03);
  return num_cpu;
}


void scu_enable(int set) {
  /* Turn on or off the SCU */
  volatile unsigned int* ctl_reg = SCU_CONTROL_REG;
  volatile unsigned int* inv_reg = SCU_INVALIDATE_ALL_REG;
  if (set) {
    /* Invalidates all the tags rams */
    *inv_reg = 0xffff;

    /* Turn on the SCU */
    *ctl_reg |= SCU_EN;
  }

  else *ctl_reg &= ~SCU_EN;
}



/*************************************************************************
  IDU
  ***********************************************************************/

void send_soft_interrupt(int cpu, int interrupt_number)
{
  volatile int *idu = (int*)IDU_BASE;
  idu[cpu] |= 1 << interrupt_number;
}


void clearall_soft_interrupt(void)
{
  volatile int *idu = (int*)IDU_BASE;
  idu[0] = 0;
  idu[1] = 0;
  idu[2] = 0;
  idu[3] = 0;
}

int is_soft_int_set(int intNum)
{
  volatile int *idu = (int*)IDU_BASE;
  return (idu[get_cpuid()] >> intNum) & 0x1;
}



/*************************************************************************
  MALLOC
  ***********************************************************************/
void init_heap_manager(void * bottom_of_heap, size_t heap_size) {

  hmngr.bottom_of_heap      = bottom_of_heap;
  hmngr.heap_size           = heap_size;
  hmngr.first_block         = (block_manager *)bottom_of_heap;
  hmngr.first_block->prev   = NULL;
  hmngr.first_block->next   = NULL;
  hmngr.first_block->data   = (char *)bottom_of_heap + sizeof(block_manager);
  hmngr.first_block->isfree = 1;
  hmngr.first_block->size   = heap_size - sizeof(block_manager);
  hmngr.lock = FREE;

}



void insert_block(block_manager* blk, size_t size) {

  block_manager *tmp_blk;

  blk->isfree = 0;

  /* Fragment block if there is enough space for a new block including data */
  if (blk->size > size + sizeof(block_manager)) {
    tmp_blk = blk->next;
    blk->next = (block_manager *) ((char *) (blk + 1) + size); /* Create new block
								  without malloc */
    blk->next->isfree = 1;
    blk->next->prev = blk;
    blk->next->next = tmp_blk;
    blk->next->data = (void *) ((int)blk->next + sizeof(block_manager));
    blk->next->size = blk->size - (size + sizeof(block_manager));
    blk->size = size;
    if (tmp_blk != NULL) tmp_blk->prev = blk->next;
  }
}


void * a_malloc(size_t size) {

  block_manager *blk;

  get_semaphore(&hmngr.lock);

  while ((size & 0x000000003) != 0)
    ++size; /* Force alignment on an int */

  for (blk = hmngr.first_block;; blk = blk->next)

    if (blk->isfree && blk->size >= size) {
      insert_block(blk, size);
      release_semaphore(&hmngr.lock);
      return (blk->data);
    }
    else if (blk->next == NULL) {
      release_semaphore(&hmngr.lock);
      return (blk->next);
    }
}




void print_heap_state(void) {
  block_manager *blk;

  if (get_cpuid() != 0) while (1);
  a_printf("Heap manager @0x%08x\n", hmngr);
  a_printf("CPU%d @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\nblck       @ data      @ size      @ state\n", get_cpuid());

  for (blk = hmngr.first_block; !!blk; blk = blk->next) {
    a_printf("0x%08x  0x%08x  0x%08x  ", blk, blk->data, blk->size);
    if (blk->isfree) a_printf(" Free\n");
    else a_printf(" Occ.\n");
  }
}




void merge_block_with_upper(block_manager *blk) {
  block_manager *tmp;

  tmp = blk->next;
  if (tmp->next != NULL) tmp->next->prev = blk;
  blk->next = tmp->next;
  blk->size += tmp->size + sizeof(block_manager);
}



void a_free(void *ptr) {
  block_manager *blk;

  get_semaphore(&hmngr.lock);
  blk = hmngr.first_block;
  while ((blk->data != ptr) && (blk->next != NULL)) blk = blk->next;

  if (blk->data != ptr) {
    a_printf("Free error: trying to free a block that doesn't exist @0x%08x \n", ptr);
    print_heap_state();
  }
  else {
    blk->isfree = 1;
    if ((blk->next != NULL) && blk->next->isfree) merge_block_with_upper(blk);
    if ((blk->prev != NULL) && blk->prev->isfree) merge_block_with_upper(blk->prev);
  }
  release_semaphore(&hmngr.lock);
}






// void a_printf(const char *str, ...) {
//   va_list ap;
//   va_start(ap, str);

//   //get_semaphore(&printf_mutex);
//   vprintf(str, ap);
//   //release_semaphore (&printf_mutex);
// }

 

size_t strlen(const char *s)
{
  const char *p = s;
  while (*p)
    p++;
  return p - s;
}

size_t strnlen(const char *s, size_t n)
{
  const char *p = s;
  while (n-- && *p)
    p++;
  return p - s;
}

int strcmp(const char* s1, const char* s2)
{
  unsigned char c1, c2;

  do {
    c1 = *s1++;
    c2 = *s2++;
  } while (c1 != 0 && c1 == c2);

  return c1 - c2;
}

char* strcpy(char* dest, const char* src)
{
  char* d = dest;
  while ((*d++ = *src++))
    ;
  return dest;
}

long atol(const char* str)
{
  long res = 0;
  int sign = 0;

  while (*str == ' ')
    str++;

  if (*str == '-' || *str == '+') {
    sign = *str == '-';
    str++;
  }

  while (*str) {
    res *= 10;
    res += *str++ - '0';
  }

  return sign ? -res : res;
}


// int putchar_custom(int ch)
// {
//   //static __thread char buf[64] __attribute__((aligned(64)));
//   static char* buf = (char *)0x13000000;
//   static int buflen = 0;

//   buf[buflen++] = ch;

//   if (ch == '\n' || buflen == sizeof(buf))
//   {
//     //syscall(SYS_write, 1, (uintptr_t)buf, buflen);
//   buf[buflen++] = ch;
//     buflen = 0;
//   }
//   return 0;
// }

void putchar_custom(char ch)
{
  char* buf = (char *)0x22200f00;
  *buf = ch;
}

void printhex(unsigned long x)
{
  char str[17];
  int i;
  for (i = 0; i < 16; i++)
  {
    str[15-i] = (x & 0xF) + ((x & 0xF) < 10 ? '0' : 'a'-10);
    x >>= 4;
    putchar_custom(str[15-i]);
  }
  str[16] = 0;
  putchar_custom(str[16]);
}

static void printnum(void (*putch)(int, void**), void **putdat, unsigned long long num, unsigned base, int width, int padc)
{
  unsigned digs[sizeof(num)* 8];
  int pos = 0;

  while (1)
  {
    digs[pos++] = num % base;
    if (num < base)
      break;
    num /= base;
  }

  while (width-- > pos)
    putch(padc, putdat);

  while (pos-- > 0)
    putch(digs[pos] + (digs[pos] >= 10 ? 'a' - 10 : '0'), putdat);
}

static unsigned long long getuint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, unsigned long long);
  else if (lflag)
    return va_arg(*ap, unsigned long);
  else
    return va_arg(*ap, unsigned int);
}

static long long getint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, long long);
  else if (lflag)
    return va_arg(*ap, long);
  else
    return va_arg(*ap, int);
}

static void vprintfmt(void (*putch)(int, void**), void **putdat, const char *fmt, va_list ap)
{
  register const char* p;
  const char* last_fmt;
  register int ch, err;
  unsigned long long num;
  int base, lflag, width, precision, altflag;
  char padc;

  while (1) {
    while ((ch = *(unsigned char *) fmt) != '%') {
      if (ch == '\0')
        return;
      fmt++;
      putch(ch, putdat);
    }
    fmt++;

    // Process a %-escape sequence
    last_fmt = fmt;
    padc = ' ';
    width = -1;
    precision = -1;
    lflag = 0;
    altflag = 0;
  reswitch:
    switch (ch = *(unsigned char *) fmt++) {

    // flag to pad on the right
    case '-':
      padc = '-';
      goto reswitch;
      
    // flag to pad with 0's instead of spaces
    case '0':
      padc = '0';
      goto reswitch;

    // width field
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      for (precision = 0; ; ++fmt) {
        precision = precision * 10 + ch - '0';
        ch = *fmt;
        if (ch < '0' || ch > '9')
          break;
      }
      goto process_precision;

    case '*':
      precision = va_arg(ap, int);
      goto process_precision;

    case '.':
      if (width < 0)
        width = 0;
      goto reswitch;

    case '#':
      altflag = 1;
      goto reswitch;

    process_precision:
      if (width < 0)
        width = precision, precision = -1;
      goto reswitch;

    // long flag (doubled for long long)
    case 'l':
      lflag++;
      goto reswitch;

    // character
    case 'c':
      putch(va_arg(ap, int), putdat);
      break;

    // string
    case 's':
      if ((p = va_arg(ap, char *)) == NULL)
        p = "(null)";
      if (width > 0 && padc != '-')
        for (width -= strnlen(p, precision); width > 0; width--)
          putch(padc, putdat);
      for (; (ch = *p) != '\0' && (precision < 0 || --precision >= 0); width--) {
        putch(ch, putdat);
        p++;
      }
      for (; width > 0; width--)
        putch(' ', putdat);
      break;

    // (signed) decimal
    case 'd':
      num = getint(&ap, lflag);
      if ((long long) num < 0) {
        putch('-', putdat);
        num = -(long long) num;
      }
      base = 10;
      goto signed_number;

    // unsigned decimal
    case 'u':
      base = 10;
      goto unsigned_number;

    // (unsigned) octal
    case 'o':
      // should do something with padding so it's always 3 octits
      base = 8;
      goto unsigned_number;

    // pointer
    case 'p':
      //static_assert(sizeof(long) == sizeof(void*));
      lflag = 1;
      putch('0', putdat);
      putch('x', putdat);
      /* fall through to 'x' */

    // (unsigned) hexadecimal
    case 'x':
      base = 16;
    unsigned_number:
      num = getuint(&ap, lflag);
    signed_number:
      printnum(putch, putdat, num, base, width, padc);
      break;

    // escaped '%' character
    case '%':
      putch(ch, putdat);
      break;
      
    // unrecognized escape sequence - just print it literally
    default:
      putch('%', putdat);
      fmt = last_fmt;
      break;
    }
  }
}

void a_printf(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  vprintfmt((void*)putchar_custom, 0, fmt, ap);

  va_end(ap);
}

/*************************************************************************
  init_smp_system (void)

  This function is run by the CPU0 alone
  Please make any global init in this function, while the other processors
  are still blocked in the init.s
  ***********************************************************************/

void init_smp_system(void)
{
  int i;
  printf_mutex = FREE;

  /* init the memory barriers */
  wbwas_barrier.lock = FREE;
  wbwas_barrier.count = 0;
  wbwas_barrier.release = 0;

  if (gic_present()) {
  /* Look at how many processors we are */
  how_many_of_us = get_number_of_cpu();

  /* unlock the other processors */
  for (i= 1; i < how_many_of_us; ++i)
    send_soft_interrupt(i, 1);
  }
  else /* uniprocessor config */
    how_many_of_us = 1;

}

/*************************************************************************
   gic_present(void) {
   Check if we have a GIC, to differenciate between falcon_up and falcon_mp build
  ***********************************************************************/

int gic_present(void) {
  volatile unsigned int *primecell0 = (unsigned *)(GIC_DIST_BASE + INT_PRIMECELL_ID0);
  volatile unsigned int *primecell1 = (unsigned *)(GIC_DIST_BASE + INT_PRIMECELL_ID1);
  volatile unsigned int *primecell2 = (unsigned *)(GIC_DIST_BASE + INT_PRIMECELL_ID2);
  volatile unsigned int *primecell3 = (unsigned *)(GIC_DIST_BASE + INT_PRIMECELL_ID3);
  volatile unsigned int *periph0 = (unsigned *)(GIC_DIST_BASE + INT_PERIPH_ID0);
  volatile unsigned int *periph1 = (unsigned *)(GIC_DIST_BASE + INT_PERIPH_ID1);
  volatile unsigned int *periph2 = (unsigned *)(GIC_DIST_BASE + INT_PERIPH_ID2);

  if (*primecell0 == 0x0d && *primecell1 == 0xf0 && *primecell2 == 0x05 && *primecell3 == 0xb1 &&
        *periph0 == 0x90 && *periph1 == 0xb3 && *periph2 == 0x1b)    // Falcon IDs
    return 1;
  else
    return 0;
}

#define EMMC_IO_BASE 0x25001000

void emmc_wio_32(unsigned int addr, unsigned int wdata) {
    *(volatile unsigned int *)(EMMC_IO_BASE | addr) = wdata;
    a_printf("EMMC:  write address 0x%x data 0x%x\n",addr,wdata);
}
void emmc_wio_16(unsigned int addr, unsigned short wdata) {
    *(volatile unsigned short *)(EMMC_IO_BASE | addr) = wdata;
    a_printf("EMMC:  write address 0x%x data 0x%x\n",addr,wdata);
}
void emmc_wio_8(unsigned int addr, unsigned char wdata) {
    *(volatile unsigned char *)(EMMC_IO_BASE | addr) = wdata;
    a_printf("EMMC:  write address 0x%x data 0x%x\n",addr,wdata);
}
void emmc_rio_32(unsigned int addr, unsigned int *rdata) {
    *rdata = *(volatile unsigned int *)(EMMC_IO_BASE | addr);
    a_printf("EMMC:  read address 0x%x data 0x%x\n",addr,*rdata);
}
void emmc_rio_16(unsigned int addr, unsigned short *rdata) {
    *rdata = *(volatile unsigned short *)(EMMC_IO_BASE | addr);
    a_printf("EMMC:  read address 0x%x data 0x%x\n",addr,*rdata);
}
void emmc_rio_8(unsigned int addr, unsigned char *rdata) {
    *rdata = *(volatile unsigned char *)(EMMC_IO_BASE | addr);
    a_printf("EMMC:  read address 0x%x data 0x%x\n",addr,*rdata);
}
void emmc_rio_32_expect(unsigned int addr, unsigned int expect_data, unsigned int mask, int max_count) {
    int i;
    unsigned int rdata;
    unsigned int rdata_masked;
    int val;
    val = 0;
    max_count *= 2;
    for(i=0;i<max_count;i=i+1){
        emmc_rio_32(addr,&rdata);
        rdata_masked = rdata & mask;
        a_printf("i: %d, rdata: 0x%x, rdata_masked: 0x%x, expect data: 0x%x\n",i,rdata,rdata_masked,expect_data);
        if(rdata_masked == expect_data){
            val = 1;
            break;
        }
    }
    if(val == 0){
        a_printf("EMMC:  read address 0x%x expect data 0x%x fail!\n",addr,expect_data);
    }else{
        a_printf("EMMC:  read address 0x%x expect data 0x%x at %d times success!\n",addr,expect_data,i);
    }
}
void emmc_rio_16_expect(unsigned int addr, unsigned short expect_data, unsigned short mask, int max_count) {
    int i;
    unsigned short rdata;
    unsigned short rdata_masked;
    int val;
    val = 0;
    max_count *= 2;
    for(i=0;i<max_count;i=i+1){
        emmc_rio_16(addr,&rdata);
        rdata_masked = rdata & mask;
        a_printf("i: %d, rdata: 0x%x, rdata_masked: 0x%x, expect data: 0x%x\n",i,rdata,rdata_masked,expect_data);
        if(rdata_masked == expect_data){
            val = 1;
            break;
        }
    }
    if(val == 0){
        a_printf("EMMC:  read address 0x%x expect data 0x%x fail!\n",addr,expect_data);
    }else{
        a_printf("EMMC:  read address 0x%x expect data 0x%x at %d times success!\n",addr,expect_data,i);
    }
}
void emmc_rio_8_expect(unsigned int addr, unsigned char expect_data, unsigned char mask, int max_count) {
    int i;
    unsigned char rdata;
    unsigned char rdata_masked;
    int val;
    val = 0;
    max_count *= 2;
    for(i=0;i<max_count;i=i+1){
        emmc_rio_8(addr,&rdata);
        rdata_masked = rdata & mask;
        a_printf("i: %d, rdata: 0x%x, rdata_masked: 0x%x, expect data: 0x%x\n",i,rdata,rdata_masked,expect_data);
        if(rdata_masked == expect_data){
            val = 1;
            break;
        }
    }
    if(val == 0){
        a_printf("EMMC:  read address 0x%x expect data 0x%x fail!\n",addr,expect_data);
    }else{
        a_printf("EMMC:  read address 0x%x expect data 0x%x at %d times success!\n",addr,expect_data,i);
    }
}

void send_emmc_cmd(unsigned int cmd_index, unsigned int cmd_type, unsigned data_present_sel, unsigned int idx_check, unsigned int crc_check, unsigned int sub_cmd, unsigned int resp_type, unsigned int argument){
  unsigned int rdata_32;
  unsigned short rdata_16;
  unsigned short cmd_r;
  cmd_r = (cmd_index<<8) | (cmd_type<<6) | (data_present_sel<<5) | (idx_check<<4) | (crc_check<<3) | (sub_cmd<<2) | resp_type;
  a_printf("-------EMMC start send CMD%0d\n",cmd_index);
  emmc_wio_32(0x8,argument);      //ARGUMENT
  emmc_wio_16(0xe,cmd_r);         //CMD
  //emmc_rio_16(0x30,&rdata_16);    //NORMAL_INT_STAT_R
  emmc_rio_16_expect(0x30,0x1,0x1,100);    //NORMAL_INT_STAT_R
  emmc_rio_16(0x32,&rdata_16);    //ERROR_INT_STAT_R
  if(rdata_16 != 0){
    a_printf("send CMD%0d error, error code is 0x%x\n",cmd_index,rdata_16);
  }
  emmc_rio_32(0x190,&rdata_32);   //CQIS
  emmc_rio_32(0x10,&rdata_32);    //RESP01
  emmc_rio_32(0x14,&rdata_32);    //RESP23
  emmc_rio_32(0x18,&rdata_32);    //RESP45
  emmc_rio_32(0x1c,&rdata_32);    //RESP67
  emmc_wio_16(0x30,0x1);
  emmc_wio_16(0x32,0xffff);

  a_printf("-------SD send CMD%0d finish\n",cmd_index);
}

void emmc_init(){
  unsigned int rdata_32;
  unsigned short rdata_16;
  unsigned char rdata_8;
  unsigned int wdata_32;
  unsigned short wdata_16;
  unsigned char wdata_8;
  unsigned int rca;
  unsigned int block_len;
  unsigned int block_count;
  unsigned int* wr_data_addr;
  unsigned int* rd_data_addr;
  int error_flag;
  int i,j;

  /**************************************************/
  /*   Controller       Registers start at 0x000    */
  /*   Vendor2          Registers start at 0x180    */
  /*     PHY            Registers start at 0x300    */
  /*   Vendor1          Registers start at 0x500    */
  /*   Embedded control Registers start at 0xf6c    */
  /**************************************************/

//SW_RST_R.SW_RST_DAT.set(1),SW_RST_R.SW_RST_CMD.set(1)
  emmc_wio_8(0x2f,0x6);
//Wait SW_RST_R.SW_RST_DAT 0
  emmc_rio_8_expect(0x2f,0,0x6,10);
//Wait PHY_CNFG.PHY_PWRGOOD 1
  emmc_rio_32_expect(0x300,2,0x2,10);
//PHY_CNFG.PAD_SN.set(4'b1000),PHY_CNFG.PAD_SP.set(4'b1001)
  emmc_wio_32(0x300,0x890002);
//program_phy_registers
//CMDPAD_CNFG.WEAKPULL_EN.set(2'b01),CMDPAD_CNFG.RXSEL.set(3'b001)
  emmc_wio_16(0x304,0x449);
//DATPAD_CNFG.WEAKPULL_EN.set(2'b01),DATPAD_CNFG.RXSEL.set(3'b001)
  emmc_wio_16(0x306,0x449);
//CLKPAD_CNFG.WEAKPULL_EN.set(2'b01),CLKPAD_CNFG.RXSEL.set(3'b001)
  emmc_wio_16(0x308,0x441);
//STBPAD_CNFG.WEAKPULL_EN.set(2'b01),STBPAD_CNFG.RXSEL.set(3'b001)
  emmc_wio_16(0x30a,0x451);
//RSTNPAD_CNFG.WEAKPULL_EN.set(2'b01),RSTNPAD_CNFG.RXSEL.set(3'b001)
  emmc_wio_16(0x30c,0x449);
//SMPLDL_CNFG.INPSEL_CNFG.set(2'b10);
//SMPLDL_CNFG.BYPASS_EN.set(1'b0);
//SMPLDL_CNFG.EXTDLY_EN.set(1'b0);
  emmc_wio_8(0x320,0x8);
//ATDL_CNFG
  emmc_wio_8(0x321,0x8);
//PHY_CNFG.PHY_RSTN.set(1'b1)
  emmc_wio_32(0x300,0x890003);
  emmc_wio_8(0x2f,0x6);
//NORMAL_INT_STAT_EN_R
  emmc_wio_16(0x34,0xc0);
  emmc_wio_16(0x38,0xc0);
  emmc_rio_32_expect(0x24,0x30000,0x30000,10);
  emmc_wio_16(0x30,0x40);
  emmc_wio_16(0x38,0x0);
  emmc_wio_8(0x29,0xe);
  //HOST_CTRL1_R
  emmc_wio_8(0x28,0x22);    // 8 bits mode, sdma
  emmc_wio_8(0x29,0xbe);
  emmc_wio_16(0x3e,0x7400);
  emmc_wio_8(0x29,0xbf);
  emmc_wio_16(0x3e,0x7403);
  emmc_rio_8(0x508,&rdata_8);//MSHC_CTRL_R
  emmc_wio_16(0x52c,0xd);//EMMC_CTRL_R
  emmc_wio_16(0x2c,0x1);
  emmc_wio_16(0x2c,0xb);
  emmc_wio_16(0x2c,0x10f);//CLK_CTRL_R
  emmc_wio_16(0x34,0x60ff);
  emmc_wio_16(0x38,0x60fb);
  emmc_wio_16(0x36,0xfff);
  emmc_wio_16(0x3a,0xfff);
  emmc_wio_32(0x540,0x1f0b0004);
  emmc_wio_16(0x4,0x4008);
  emmc_wio_32(0x0,0x5dddc9c8);
  emmc_wio_16(0xc,0x12);

  for(i=0;i<80;i=i+1){
      a_printf("wait %d of 80...\n",i);
  }

  emmc_wio_16(0x2c,0x04f);//CLK_CTRL_R

  //CMD0,   GO_PRE_IDLE_STATE
//  send_emmc_cmd(0, 0, 0, 0, 0, 0, 0, 0xf0f0f0f0);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd,resp_type,argument
  //CMD0,   BOOT_INITIATION
  //send_emmc_cmd(0, 0, 0, 0, 0, 0, 0, 0xfffffffa);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd,resp_type,argument
  //CMD0,   GO_IDLE_STATE
  //send_emmc_cmd(0, 0, 0, 0, 0, 0, 0, 0);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd,resp_type,argument


  //CMD1, SEND_OP_COND
  send_emmc_cmd(1, 0, 0, 0, 0, 0, 0x2, 0x0);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd,resp_type,argument


  //CMD1, SEND_OP_COND
  send_emmc_cmd(1, 0, 0, 0, 0, 0, 0x2, 0xc0ff8080);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd,resp_type,argument

  //CMD2,   ALL_SEND_CID
  send_emmc_cmd(2, 0, 0, 0, 0, 0, 0x1, 0x0);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD3,   SEND_RELATIVE_ADDR
  send_emmc_cmd(3, 0, 0, 0, 0, 0, 0x2, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  emmc_wio_16(0x2c,0x40f);//CLK_CTRL_R

  //CMD10, SEND_CID
  send_emmc_cmd(10, 0, 0, 0, 0, 0, 0x1, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD9,   SEND_CSD
  send_emmc_cmd(9, 0, 0, 0, 0, 0, 0x1, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD4,   SET_DSR
  send_emmc_cmd(4, 0, 0, 0, 0, 0, 0, 0x46b90000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD5,   SLEEP/AWAKE
  send_emmc_cmd(5, 0, 0, 0, 0, 0, 0x2, 0x18000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument
  //CMD5,   SLEEP/AWAKE
  send_emmc_cmd(5, 0, 0, 0, 0, 0, 0x2, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD40,   GO_IRQ_STATE
  send_emmc_cmd(40, 0, 0, 0, 0, 0, 0x2, 0x0);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument
  //CMD55
  send_emmc_cmd(55, 0, 0, 0, 0, 0, 0x2, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD7,   SELECT_CARD
  send_emmc_cmd(7, 0, 0, 0, 0, 0, 0x2, 0x10000);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  //CMD6,  SWITCH, SET_BUS_WIDTH
  send_emmc_cmd(6, 0, 0, 0, 0, 0, 0x2, 0x3b70204);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument
  //CMD6,  SWITCH, SET_HS_TIMING
  send_emmc_cmd(6, 0, 0, 0, 0, 0, 0x2, 0x3b90204);//cmd_index,cmd_type,data_present_sel,idx_check,crc_check,sub_cmd, resp_type,argument

  emmc_wio_16(0x2c,0x40f);//CLK_CTRL_R


  //Start Tuning
  a_printf("Set HOST_CTRL2_R.EXEC_TUNING=1\n",i);
  //HOST_CTRL2_R
  emmc_rio_16(0x3e,&rdata_16);
  emmc_wio_16(0x3e,rdata_16|0x40);

  emmc_wio_8(0x2e,3); //TOUT_CTRL_R
  emmc_wio_16(0x4,128);//BLOCKSIZE_R
  emmc_wio_32(0x0,0x1);//Block Count
  emmc_wio_16(0xc,0x32);//XFER_MODE_R
  emmc_wio_32(0x8,0x0);//ARGUEMENT_R

  for(i=0;i<128;i=i+1){
    a_printf("Issue tuning command (CMD21) %d times\n",i);
    emmc_wio_16(0xe,0x153a);//CMD_R

    for(j=0;j<240;j=j+1){
      a_printf("wait %d of 120...\n",j);
    }

    a_printf("Check if NORMAL_INT_STAT_R.BUF_RD_READY = 1?\n");
    emmc_rio_16_expect(0x30,0x20,0x20,200);    //NORMAL_INT_STAT_R
    emmc_wio_16(0x30,0x20);    //NORMAL_INT_STAT_R
  }

  a_printf("Check if HOST_CTRL2_R.EXEC_TUNING = 1?\n");
  emmc_rio_16_expect(0x3e,0x0,0x40,200);    //HOST_CTRL2_R
  a_printf("Check if HOST_CTRL2_R.SAMPLE_CLK_SEL=1?\n");
  emmc_rio_16_expect(0x3e,0x80,0x80,200);    //HOST_CTRL2_R

}

/*************************************************************************
  int main(void)
  ***********************************************************************/

int main(int argc, char *argv[]) {
 
  
  int a = 1;
  int* b = (int *) 0x22010000;
  //volatile unsigned int* ddr_addr = (unsigned int *)0x80000000;
  unsigned int* ddr_addr = (unsigned int *)0x80000000;
  int ddr_data;
  // int* c = (int *) 0x00060000;


  a_printf("hello_wrold_vmem!!!\n Please, do not use volatile keyword casually(you can use it for peripheral memory)\n");
  //for(a = 0; a < 10; a++){
  //  //*addr = d;
  //  a_printf("%x, %d :hello world!!\n", &a, a);
  //}

 
  a_printf("qspi:0x%x\n", *b);

  *(unsigned int *)0x80000000 = 0xaa;
  a_printf("ddr:0x%x\n",*ddr_addr);

  emmc_init();
 
  return (0);
}
