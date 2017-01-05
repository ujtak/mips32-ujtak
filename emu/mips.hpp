#ifndef _MIPS_H
#define _MIPS_H

#include <vector>
#include <string>

using std::vector;
using std::string;

class MIPS
{
public:

  MIPS();

  int read_inst(string src);
  int exec_cycle();
  int write_data(string dst);

  vector<string>  mem_inst;
  vector<int>     mem_data;
  vector<int>     reg_file;

private:
  const int dwidth  = 32;
  const int isize   = 8;
  const int dsize   = 8;
  const int rsize   = 5;
  const int opwidth = 6;
  const int shamt   = 5;
  const int funct   = 5;

  int pcounter = 0;
};

#include "mips.cpp"
#endif
