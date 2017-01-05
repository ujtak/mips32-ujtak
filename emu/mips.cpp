#ifdef _MIPS_H

#include <fstream>
#include <cmath>
#include "mips.hpp"

MIPS::MIPS()
{
  mem_inst.resize((int)pow(2, isize));
  mem_data.resize((int)pow(2, dsize));
  reg_file.resize((int)pow(2, rsize));
}

int MIPS::read_inst(string src)
{
  std::ifstream ifs(src);

  for (auto entry : mem_inst)
    ifs >> entry;

  return 0;
}

int MIPS::exec_cycle()
{
  return 1;
}

int MIPS::write_data(string dst)
{
  std::ofstream ofs(dst);

  for (auto entry : mem_data)
    ofs << entry << std::endl;

  return 0;
}

#endif
