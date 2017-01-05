#ifndef _MIPS_H
#define _MIPS_H

#include <vector>
#include <string>

using std::vector;
using std::string;

class MIPS
{
public:
  static const int dwidth = 32;
  static const int isize  = 8;
  static const int dsize  = 8;
  static const int rsize  = 5;
  static const int oplen  = 6;
  static const int shlen  = 5;
  static const int fnlen  = 5;

  MIPS();

  void read_inst(string src);
  int  exec_step();
  void write_data(string dst);

private:
  int pcounter = 0;

  vector<string>  mem_inst;
  vector<int>     mem_data;
  vector<int>     reg_file;

  inline void _and(int rd, int rs, int rt);
  inline void _or(int rd, int rs, int rt);
  inline void _add(int rd, int rs, int rt);
  inline void _sub(int rd, int rs, int rt);
  inline void _lw(int rs, int rt, int cv);
  inline void _sw(int rs, int rt, int cv);
  inline void _beq(int rs, int rt, int cv);
  inline void _j(int cv);
};

#include "mips.cpp"
#endif
