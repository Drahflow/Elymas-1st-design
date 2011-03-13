#include "opcodes.h"

#include <cassert>

namespace opcode {
  struct RegisterRIP {
  };

  struct Register64 {
    Register64(int number): number(number) { }
    int number;
  };

  struct Register32 {
    Register32(int number): number(number) { }
    int number;
  };

  // return the REX prefix with the bits set as necessary
  // for encoding the given width, register, index-register
  // and r/m or opcode-based register
  unsigned char rex(int width, Register64 *reg, Register64 *index,
      Register64 *rmbase) {
    return 0x40 +
      0x08 * !!width +
      0x04 * (reg && reg->number >= 8) +
      0x02 * (index && index->number >= 8) +
      0x01 * (rmbase && rmbase->number >= 8);
  }

  unsigned char rex(int width, Register32 *reg, Register64 *index,
      Register64 *rmbase) {
    return rex(width, reinterpret_cast<Register64 *>(reg), index, rmbase);
  }

  unsigned char rex(int width, int, Register64 *index,
      Register64 *rmbase) {
    return rex(width, reinterpret_cast<Register64 *>(0), index, rmbase);
  }

  unsigned char rmbyte(int opcode, Register64 *rm) {
    return 0xC0 +
      (opcode % 8) * 0x08 +
      (rm->number % 8) * 0x01;
  }

  unsigned char rmbyte(Register64 *reg, Register64 *rm) {
    return 0xC0 +
      (reg->number % 8) * 0x08 +
      (rm->number % 8) * 0x01;
  }

  int addrsize(int regnum, const Memory &mem) {
    if(mem.ripRelative) {
      if(mem.index) {
        assert(false);
      } else {
        return 5;
      }
    } else if(!mem.base) {
      assert(false);
    } else if(!mem.displacement) {
      if(mem.index) {
        if(mem.base->number != 5) {
          return 2;
        } else {
          assert(false);
        }
      } else if(mem.base->number != 4 && mem.base->number != 5) {
        return 1;
      } else if(mem.base->number == 5) {
        return 2;
      } else {
        assert(false);
      }
    } else if(mem.displacement <= 127 && mem.displacement >= -128) {
      if(mem.index) {
        assert(false);
      } else if(mem.base->number != 4) {
        return 2;
      } else {
        assert(false);
      }
    } else {
      if(mem.index) {
        assert(false);
      } else if(mem.base->number != 4) {
        return 5;
      } else {
        assert(false);
      }
    }
  }

  int addrsize(Register64 *reg, const Memory &mem) {
    return addrsize(reg->number, mem);
  }

  int addrsize(Register32 *reg, const Memory &mem) {
    return addrsize(reg->number, mem);
  }

  void addrbytes(unsigned char *target, int regnum, const Memory &mem) {
    target[0] = (regnum % 8) * 0x08;
    
    assert(!mem.ripRelative || !mem.base);

    if(mem.ripRelative) {
      if(mem.index) {
        assert(false);
      } else {
        target[0] |= 5;
        *reinterpret_cast<int32_t *>(target + 1) = mem.displacement;
      }
    } else if(!mem.base) {
      assert(false);
    } else if(!mem.displacement) {
      if(mem.index) {
        target[0] |= 4;
        if(mem.base->number != 5) {
          target[1] = mem.base->number % 8;
          target[1] |= (mem.index->number % 8) * 0x8;
          switch(mem.multiplier) {
            case 1: break;
            case 2: target[1] |= 0x40; break;
            case 4: target[1] |= 0x80; break;
            case 8: target[1] |= 0xC0; break;
            default: assert(false);
          }
        } else {
          assert(false);
        }
      } else if(mem.base->number != 4 && mem.base->number != 5) {
        target[0] |= mem.base->number % 8;
      } else if(mem.base->number == 5) {
        target[0] |= 0x45;
        target[1] = 0;
      } else {
        assert(false);
      }
    } else if(mem.displacement <= 127 && mem.displacement >= -128) {
      target[0] |= 0x40;

      if(mem.index) {
        target[0] |= 4;
        assert(false);
      } else if(mem.base->number != 4) {
        target[0] |= mem.base->number % 8;
        target[1] = mem.displacement;
      } else {
        assert(false);
      }
    } else {
      target[0] |= 0x80;

      if(mem.index) {
        target[0] |= 4;
        assert(false);
      } else if(mem.base->number != 4) {
        target[0] |= mem.base->number % 8;
        *reinterpret_cast<int32_t *>(target + 1) = mem.displacement;
      } else {
        assert(false);
      }
    }
  }

  void addrbytes(unsigned char *target, Register64 *reg, const Memory &mem) {
    addrbytes(target, reg->number, mem);
  }

  void addrbytes(unsigned char *target, Register32 *reg, const Memory &mem) {
    addrbytes(target, reg->number, mem);
  }

  Register64 *rax() { return new Register64(0); }
  Register64 *rcx() { return new Register64(1); }
  Register64 *rdx() { return new Register64(2); }
  Register64 *rbx() { return new Register64(3); }
  Register64 *rsp() { return new Register64(4); }
  Register64 *rbp() { return new Register64(5); }
  Register64 *rsi() { return new Register64(6); }
  Register64 *rdi() { return new Register64(7); }
  Register64 *r8() { return new Register64(8); }
  Register64 *r9() { return new Register64(9); }
  Register64 *r10() { return new Register64(10); }
  Register64 *r11() { return new Register64(11); }
  Register64 *r12() { return new Register64(12); }
  Register64 *r13() { return new Register64(13); }
  Register64 *r14() { return new Register64(14); }
  Register64 *r15() { return new Register64(15); }
  RegisterRIP *rip() { return new RegisterRIP(); }

  Register32 *eax() { return new Register32(0); }
  Register32 *ecx() { return new Register32(1); }
  Register32 *edx() { return new Register32(2); }
  Register32 *ebx() { return new Register32(3); }
  Register32 *esp() { return new Register32(4); }
  Register32 *ebp() { return new Register32(5); }
  Register32 *esi() { return new Register32(6); }
  Register32 *edi() { return new Register32(7); }
  Register32 *r8d() { return new Register32(8); }
  Register32 *r9d() { return new Register32(9); }
  Register32 *r10d() { return new Register32(10); }
  Register32 *r11d() { return new Register32(11); }
  Register32 *r12d() { return new Register32(12); }
  Register32 *r13d() { return new Register32(13); }
  Register32 *r14d() { return new Register32(14); }
  Register32 *r15d() { return new Register32(15); }

  Memory memory(Register64 *base) {
    Memory ret;
    ret.base = base;
    ret.displacement = 0;
    ret.index = 0;
    ret.multiplier = 0;
    ret.ripRelative = false;
    return ret;
  }

  Memory memory(int32_t disp, Register64 *base) {
    Memory ret;
    ret.base = base;
    ret.displacement = disp;
    ret.index = 0;
    ret.multiplier = 0;
    ret.ripRelative = false;
    return ret;
  }

  Memory memory(Register64 *base, Register64 *index, int multiplier) {
    Memory ret;
    ret.base = base;
    ret.displacement = 0;
    ret.index = index;
    ret.multiplier = multiplier;
    ret.ripRelative = false;
    return ret;
  }

  Memory memory(RegisterRIP *) {
    Memory ret;
    ret.base = 0;
    ret.displacement = 0;
    ret.index = 0;
    ret.multiplier = 0;
    ret.ripRelative = true;
    return ret;
  }

  Memory memory(int32_t disp, RegisterRIP *) {
    Memory ret;
    ret.base = 0;
    ret.displacement = disp;
    ret.index = 0;
    ret.multiplier = 0;
    ret.ripRelative = true;
    return ret;
  }

  Opcode *move(uint64_t val, Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(uint64_t val, Register64 *reg): val(val), reg(reg) { }
        int size(int) { return 10; }
        void write(unsigned char *target) {
          target[0] = rex(1, 0, 0, reg);
          target[1] = 0xB8 + reg->number % 8;
          *reinterpret_cast<uint64_t *>(target + 2) = val;
        }

      private:
        uint64_t val;
        Register64 *reg;
    }) (val, reg);
  }

  Opcode *move(Register64 *reg1, Register64 *reg2) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg1, Register64 *reg2): reg1(reg1), reg2(reg2) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, reg1, 0, reg2);
          target[1] = 0x89;
          target[2] = rmbyte(reg1, reg2);
        }

      private:
        Register64 *reg1;
        Register64 *reg2;
    }) (reg1, reg2);
  }

  Opcode *move(Register64 *reg, const Memory &mem) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg, const Memory &mem): reg(reg), mem(mem) { }
        int size(int) { return 2 + addrsize(reg, mem); }
        void write(unsigned char *target) {
          target[0] = rex(1, reg, mem.index, mem.base);
          target[1] = 0x89;
          addrbytes(target + 2, reg, mem);
        }

      private:
        Register64 *reg;
        Memory mem;
    }) (reg, mem);
  }

  Opcode *move(Register32 *reg, const Memory &mem) {
    return new (class _: public Opcode {
      public:
        _(Register32 *reg, const Memory &mem): reg(reg), mem(mem) { }
        int size(int) { return 2 + addrsize(reg, mem); }
        void write(unsigned char *target) {
          target[0] = rex(0, reg, mem.index, mem.base);
          target[1] = 0x89;
          addrbytes(target + 2, reg, mem);
        }

      private:
        Register32 *reg;
        Memory mem;
    }) (reg, mem);
  }

  Opcode *move(const Memory &mem, Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg, const Memory &mem): reg(reg), mem(mem) { }
        int size(int) { return 2 + addrsize(reg, mem); }
        void write(unsigned char *target) {
          target[0] = rex(1, reg, mem.index, mem.base);
          target[1] = 0x8B;
          addrbytes(target + 2, reg, mem);
        }

      private:
        Register64 *reg;
        Memory mem;
    }) (reg, mem);
  }

  Opcode *retnear() {
    return new (class _: public Opcode {
      public:
        int size(int) { return 1; }
        void write(unsigned char *target) {
          target[0] = 0xC3;
        }
    });
  }

  Opcode *push(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return reg->number >= 8? 2: 1; }
        void write(unsigned char *target) {
          if(reg->number >= 8) {
            target[0] = rex(0, 0, 0, reg);
            target[1] = 0x50 + reg->number % 8;
          } else {
            target[0] = 0x50 + reg->number % 8;
          }
        }
      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *pop(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return reg->number >= 8? 2: 1; }
        void write(unsigned char *target) {
          if(reg->number >= 8) {
            target[0] = rex(0, 0, 0, reg);
            target[1] = 0x58 + reg->number % 8;
          } else {
            target[0] = 0x58 + reg->number % 8;
          }
        }
      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *cld() {
    return new (class _: public Opcode {
      public:
        int size(int) { return 1; }
        void write(unsigned char *target) {
          target[0] = 0xFC;
        }
    });
  }

  Opcode *call(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return reg->number >= 8? 3: 2; }
        void write(unsigned char *target) {
          if(reg->number >= 8) {
            target[0] = rex(0, 0, 0, reg);
            target[1] = 0xFF;
            target[2] = rmbyte(2, reg);
          } else {
            target[0] = 0xFF;
            target[1] = rmbyte(2, reg);
          }
        }

      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *sub(Register64 *reg1, Register64 *reg2) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg1, Register64 *reg2): reg1(reg1), reg2(reg2) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, reg1, 0, reg2);
          target[1] = 0x29;
          target[2] = rmbyte(reg1, reg2);
        }

      private:
        Register64 *reg1;
        Register64 *reg2;
    }) (reg1, reg2);
  }

  Opcode *cmp(Register64 *reg1, Register64 *reg2) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg1, Register64 *reg2): reg1(reg1), reg2(reg2) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, reg1, 0, reg2);
          target[1] = 0x39;
          target[2] = rmbyte(reg1, reg2);
        }

      private:
        Register64 *reg1;
        Register64 *reg2;
    }) (reg1, reg2);
  }

  Opcode *add(int32_t val, Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(uint32_t val, Register64 *reg): val(val), reg(reg) { }
        int size(int) { return 7; }
        void write(unsigned char *target) {
          target[0] = rex(1, 0, 0, reg);
          target[1] = 0x81;
          target[2] = rmbyte(0, reg);
          *reinterpret_cast<uint64_t *>(target + 3) = val;
        }

      private:
        uint32_t val;
        Register64 *reg;
    }) (val, reg);
  }

  Opcode *jmp(Label *label) {
    return new (class _: public Opcode {
      public:
        _(Label *label): label(label) { }
        int size(int pos) {
          position = pos + 5;
          return 5;
        }
        void write(unsigned char *target) {
          target[0] = 0xE9;
          *reinterpret_cast<int32_t *>(target + 1) = label->offset - position;
        }

      private:
        Label *label;
        int32_t position;
    }) (label);
  }

  Opcode *jae(Label *label) {
    return new (class _: public Opcode {
      public:
        _(Label *label): label(label) { }
        int size(int pos) {
          position = pos + 6;
          return 6;
        }
        void write(unsigned char *target) {
          target[0] = 0x0F;
          target[1] = 0x83;
          *reinterpret_cast<int32_t *>(target + 2) = label->offset - position;
        }

      private:
        Label *label;
        int32_t position;
    }) (label);
  }

  Opcode *div(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, 0, 0, reg);
          target[1] = 0xF7;
          target[2] = rmbyte(6, reg);
        }

      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *mul(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, 0, 0, reg);
          target[1] = 0xF7;
          target[2] = rmbyte(4, reg);
        }

      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *enter(uint16_t frameSize) {
    return new (class _: public Opcode {
      public:
        _(uint16_t frameSize): frameSize(frameSize) { }
        int size(int) { return 4; }
        void write(unsigned char *target) {
          target[0] = 0xC8;
          *reinterpret_cast<uint16_t *>(target + 1) = frameSize;
          target[3] = 0x00;
        }

      private:
        uint16_t frameSize;
    }) (frameSize);
  }

  Opcode *leave() {
    return new (class _: public Opcode {
      public:
        int size(int) { return 1; }
        void write(unsigned char *target) {
          target[0] = 0xC9;
        }
    });
  }

  Opcode *neg(Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg): reg(reg) { }
        int size(int) { return 3; }
        void write(unsigned char *target) {
          target[0] = rex(1, 0, 0, reg);
          target[1] = 0xF7;
          target[2] = rmbyte(3, reg);
        }

      private:
        Register64 *reg;
    }) (reg);
  }

  Opcode *lea(const Memory &mem, Register64 *reg) {
    return new (class _: public Opcode {
      public:
        _(Register64 *reg, const Memory &mem): reg(reg), mem(mem) { }
        int size(int) { return 2 + addrsize(reg, mem); }
        void write(unsigned char *target) {
          target[0] = rex(1, reg, mem.index, mem.base);
          target[1] = 0x8D;
          addrbytes(target + 2, reg, mem);
        }

      private:
        Register64 *reg;
        Memory mem;
    }) (reg, mem);
  }

  Opcode *jmp(const Memory &mem) {
    return new (class _: public Opcode {
      public:
        _(const Memory &mem): mem(mem) { }
        int size(int) {
          return ((mem.index && mem.index->number >= 8) ||
            (mem.base && mem.base->number >= 8)? 3: 2)
            + addrsize(4, mem); }
        void write(unsigned char *target) {
          if((mem.index && mem.index->number >= 8) ||
            (mem.base && mem.base->number >= 8)) {
            target[0] = rex(0, 0, mem.index, mem.base);
            target[1] = 0xFF;
            addrbytes(target + 2, 4, mem);
          } else {
            target[0] = 0xFF;
            addrbytes(target + 1, 4, mem);
          }
        }

      private:
        Memory mem;
    }) (mem);
  }

  Opcode *call(const Memory &mem) {
    return new (class _: public Opcode {
      public:
        _(const Memory &mem): mem(mem) { }
        int size(int) {
          return ((mem.index && mem.index->number >= 8) ||
            (mem.base && mem.base->number >= 8)? 3: 2)
            + addrsize(2, mem); }
        void write(unsigned char *target) {
          if((mem.index && mem.index->number >= 8) ||
            (mem.base && mem.base->number >= 8)) {
            target[0] = rex(0, 0, mem.index, mem.base);
            target[1] = 0xFF;
            addrbytes(target + 2, 2, mem);
          } else {
            target[0] = 0xFF;
            addrbytes(target + 1, 2, mem);
          }
        }

      private:
        Memory mem;
    }) (mem);
  }
};
