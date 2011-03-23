#ifndef OPCODES_H
#define OPCODES_H

#include <stdint.h>
#include <cassert>

class Opcode {
  public:
    virtual ~Opcode() { };
    virtual int size(int offset) = 0;
    virtual void write(unsigned char *) = 0;
};

class Label: public Opcode {
  public:
    int offset;

    Label(): resolved(false) { }

    int size(int off) {
      offset = off;
      resolved = true;
      return 0;
    }

    void write(unsigned char *) {
      assert(resolved);
    }

  private:
    bool resolved;
};

namespace opcode {
  struct Register64;
  struct Register32;
  struct Register16;
  struct Register8;
  struct RegisterRIP;
  struct Memory {
    int32_t displacement;
    Register64 *base;
    Register64 *index;
    uint8_t multiplier;
    bool ripRelative;
  };

  Opcode *add(int32_t, Register64 *);
  Opcode *call(Register64 *);
  Opcode *call(const Memory &);
  Opcode *cld();
  Opcode *cmp(Register64 *, Register64 *);
  Opcode *div(Register64 *);
  Opcode *enter(uint16_t);
  Opcode *jae(Label *);
  Opcode *jmp(Label *);
  Opcode *jmp(const Memory &);
  Opcode *lea(const Memory &, Register64 *);
  Opcode *leave();
  Opcode *move(uint64_t, Register64 *);
  Opcode *move(Register64 *, Register64 *);
  Opcode *move(Register64 *, const Memory &);
  Opcode *move(const Memory &, Register64 *);
  Opcode *move(Register32 *, const Memory &);
  Opcode *mul(Register64 *);
  Opcode *neg(Register64 *);
  Opcode *pop(Register64 *);
  Opcode *push(Register64 *);
  Opcode *shr(uint8_t, Register64 *);
  Opcode *sub(Register64 *, Register64 *);
  Opcode *retnear();

  Register64 *rax();
  Register64 *rcx();
  Register64 *rdx();
  Register64 *rbx();
  Register64 *rsp();
  Register64 *rbp();
  Register64 *rsi();
  Register64 *rdi();
  Register64 *r8();
  Register64 *r9();
  Register64 *r10();
  Register64 *r11();
  Register64 *r12();
  Register64 *r13();
  Register64 *r14();
  Register64 *r15();
  RegisterRIP *rip();

  Register32 *eax();
  Register32 *ecx();
  Register32 *edx();
  Register32 *ebx();
  Register32 *esp();
  Register32 *ebp();
  Register32 *esi();
  Register32 *edi();
  Register32 *r8d();
  Register32 *r9d();
  Register32 *r10d();
  Register32 *r11d();
  Register32 *r12d();
  Register32 *r13d();
  Register32 *r14d();
  Register32 *r15d();

  Register16 *ax();
  Register16 *cx();
  Register16 *dx();
  Register16 *bx();
  Register16 *sp();
  Register16 *bp();
  Register16 *si();
  Register16 *di();
  Register16 *r8w();
  Register16 *r9w();
  Register16 *r10w();
  Register16 *r11w();
  Register16 *r12w();
  Register16 *r13w();
  Register16 *r14w();
  Register16 *r15w();

  Register8 *al();
  Register8 *cl();
  Register8 *dl();
  Register8 *bl();
  Register8 *spl();
  Register8 *bpl();
  Register8 *sil();
  Register8 *dil();
  Register8 *r8l();
  Register8 *r9l();
  Register8 *r10l();
  Register8 *r11l();
  Register8 *r12l();
  Register8 *r13l();
  Register8 *r14l();
  Register8 *r15l();

  Memory memory(Register64 *base);
  Memory memory(int32_t disp, Register64 *base);
  Memory memory(Register64 *base, Register64 *index, int multiplier);
  Memory memory(RegisterRIP *);
  Memory memory(int32_t disp, RegisterRIP *);
};

#endif
