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
  static const int isize  = 2;// 8;
  static const int dsize  = 4;// 8;
  static const int rsize  = 5;
  static const int oplen  = 6;
  static const int shlen  = 5;
  static const int fnlen  = 6;

  MIPS(bool verbose=false);

  void load_inst(string src);
  void load_data(string src);
  void load_reg(string src);

  void save_reg(string dst);
  void save_data(string dst);

  int exec();
  int exec_step();

private:
  bool verbose = false;
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

  template <typename T>
  void load(vector<T> &vec, string dst);
  template <typename T>
  void save(vector<T> vec, string dst);
};

#include "mips.cpp"
#endif
