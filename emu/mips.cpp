#ifdef _MIPS_H

#include <fstream>
#include <cmath>
#include "mips.hpp"

using std::ifstream;
using std::ofstream;
using std::stoi;

MIPS::MIPS()
{
  mem_inst.resize((int)pow(2, isize));
  mem_data.resize((int)pow(2, dsize));
  reg_file.resize((int)pow(2, rsize));
}

void MIPS::read_inst(string src)
{
  ifstream ifs(src);

  for (auto entry : mem_inst)
    ifs >> entry;
}

int MIPS::exec_step()
{
  auto stob = [](string x) -> int { return stoi(x, nullptr, 2); };

  string inst   = mem_inst[pcounter/4];

  int opcode = stob(inst.substr(0, oplen));

  int rsR    = stob(inst.substr(oplen,               rsize));
  int rtR    = stob(inst.substr(oplen+rsize,         rsize));
  int rdR    = stob(inst.substr(oplen+2*rsize,       rsize));
  int shamt  = stob(inst.substr(oplen+3*rsize,       shlen));
  int funct  = stob(inst.substr(oplen+3*rsize+shlen, fnlen));

  int rsI    = stob(inst.substr(oplen,         rsize));
  int rtI    = stob(inst.substr(oplen+rsize,   rsize));
  int cvI    = stob(inst.substr(oplen+2*rsize, dwidth-(oplen+2*rsize)));

  int cvJ    = stob(inst.substr(oplen, dwidth-oplen));

  switch (opcode) {
    // R-Type
    case 0x0:
      switch (funct) {
        case 0x20: _add(rdR, rsR, rtR); break;
        case 0x22: _sub(rdR, rsR, rtR); break;
        case 0x24: _and(rdR, rsR, rtR); break;
        case 0x25: _or(rdR, rsR, rtR);  break;
        default:
          std::cerr << "This operation is not implemented" << std::endl;
          exit(1);
      }
      break;

    // I-Type
    // TODO: check the order of rs and rt
    case 0x4:  _beq(rsI, rtI, cvI); break;
    case 0x23: _lw(rsI, rtI, cvI);  break;
    case 0x2b: _sw(rsI, rtI, cvI);  break;

    // J-Type
    case 0x2: _j(cvJ);   break;
    // case 0x3: _jal(cvJ); break; // not implemented

    default:
      std::cerr << "This operation is not implemented" << std::endl;
      exit(1);
  }

  return 1;
}

void MIPS::write_data(string dst)
{
  ofstream ofs(dst);

  for (auto entry : mem_data)
    ofs << entry << std::endl;
}

inline void MIPS::_and(int rd, int rs, int rt)
{
  reg_file[rd] = reg_file[rs] && reg_file[rt];
  pcounter = pcounter + 4;
}

inline void MIPS::_or(int rd, int rs, int rt)
{
  reg_file[rd] = reg_file[rs] || reg_file[rt];
  pcounter = pcounter + 4;
}

inline void MIPS::_add(int rd, int rs, int rt)
{
  reg_file[rd] = reg_file[rs] + reg_file[rt];
  pcounter = pcounter + 4;
}

inline void MIPS::_sub(int rd, int rs, int rt)
{
  reg_file[rd] = reg_file[rs] - reg_file[rt];
  pcounter = pcounter + 4;
}

inline void MIPS::_lw(int rs, int rt, int cv)
{
  reg_file[rs] = mem_data[ reg_file[rt]+cv ];
  pcounter = pcounter + 4;
}

inline void MIPS::_sw(int rs, int rt, int cv)
{
  mem_data[ reg_file[rt]+cv ] = reg_file[rs];
  pcounter = pcounter + 4;
}

inline void MIPS::_beq(int rs, int rt, int cv)
{
  pcounter = pcounter + 4 + 4 * cv;
}

inline void MIPS::_j(int cv)
{
  pcounter = ((pcounter+4) & 0xF0000000)
           | ((4 * cv)     & 0x0FFFFFFF);
}

#endif
