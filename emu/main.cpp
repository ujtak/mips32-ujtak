#include <iostream>
#include <string>
#include <cstdlib>
#include <unistd.h>
#include "mips.hpp"

using std::string;

struct config {
  string prog_name;
  bool verbose;
  string inst_file;
  string data_init;
  string reg_init;
  string data_dump;
  string reg_dump;
  int num_step;
} conf;

void usage(int exitcode)
{
  (exitcode ? std::cerr : std::cout)
    <<  "Usage: " + conf.prog_name + " [ -h ] <file>\n"
        "MIPS-subset Emulator\n"
        "\n"
        "<file>     binary-number source to execute\n"
        "  -v       verbose mode\n"
        "  -d       initial configuration of the data memory\n"
        "  -r       initial configuration of registers\n"
        "  -o       output filename\n"
        "  -s       target filename to dump registers' states\n"
        "  -c       specify the number of cycles\n"
        "  -h       print this help"
    <<  std::endl;

  exit(exitcode);
}

void parseopt(int argc, char **argv)
{
  conf.prog_name  = argv[0];
  conf.verbose    = false;
  conf.inst_file  = argv[argc-1];
  conf.data_init  = "";
  conf.reg_init   = "";
  conf.data_dump  = "mem_data_true.dat";
  conf.reg_dump   = "";
  conf.num_step   = 0;

  while (1) {
    int opt = getopt(argc, argv, "vd:r:o:s:c:h");
    if (opt == -1)
      break;

    switch (opt) {
      case 'v': conf.verbose = true; break;
      case 'd': conf.data_init = optarg; break;
      case 'r': conf.reg_init  = optarg; break;
      case 'o': conf.data_dump = optarg; break;
      case 's': conf.reg_dump  = optarg; break;
      case 'c': conf.num_step  = std::stoi(optarg); break;
      case 'h': usage(0);
      default:  usage(1);
    }
  }

  if (optind != argc-1)
    usage(1);
}

int main(int argc, char **argv)
{
  parseopt(argc, argv);

  MIPS cpu(conf.verbose);

  // Load Instructions and Initial Data
  cpu.load_inst(conf.inst_file);
  if (conf.data_init != "")
    cpu.load_data(conf.data_init);
  if (conf.reg_init != "")
    cpu.load_reg(conf.reg_init);

  // Execute Instructions on the emulator
  if (0 < conf.num_step)
    for (int i=0; i < conf.num_step; i++) {
      int ack = cpu.exec_step();
      if (ack != 0) break;
    }
  else
    cpu.exec();

  // Save Consequential Data
  cpu.save_data(conf.data_dump);
  if (conf.reg_dump != "")
    cpu.save_reg(conf.reg_dump);

  return 0;
}
