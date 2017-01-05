#include <iostream>
#include <string>
#include <cstdlib>
#include <unistd.h>
#include "mips.hpp"

using std::string;

struct config {
  string prog_name;
  string inst_file;
  string data_file;
  int num_cycle;
} conf;

void usage(int exitcode)
{
  exitcode ? std::cerr : std::cout
    <<  "Usage: " + conf.prog_name + " [ -h ] <file>\n"
        "\n"
        "<file>     binary source to execute\n"
        "  -h       print this help\n"
    <<  std::endl;

  exit(exitcode);
}

void parseopt(int argc, char **argv)
{
  conf.prog_name = argv[0];
  conf.inst_file = argv[argc-1];
  conf.data_file = "mem_data_true.dat";
  conf.num_cycle = 0;

  while (1) {
    int opt = getopt(argc, argv, "ho:c:");
    if (opt == -1)
      break;

    switch (opt) {
      case 'o': conf.data_file = optarg; break;
      case 'c': conf.num_cycle = std::stoi(optarg); break;
      case 'h': usage(0);
      default:  usage(1);
    }
  }

  if (optind != argc - 1)
    usage(1);
}

int main(int argc, char **argv)
{
  MIPS cpu;

  parseopt(argc, argv);

  cpu.read_inst(conf.inst_file);

  if (0 < conf.num_cycle)
    for (int i=0; i < conf.num_cycle; i++) {
      int ack = cpu.exec_cycle();
      if (ack != 0) break;
    }
  else
    while (1) {
      int ack = cpu.exec_cycle();
      if (ack != 0) break;
    }

  cpu.write_data(conf.data_file);

  return 0;
}
