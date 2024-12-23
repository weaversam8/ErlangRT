use crate::{
  command_line_args::ErlStartArgs,
  emulator::{atom, mfa::ModFunArgs, spawn_options::SpawnOptions, vm::VM},
  term::*,
};
use std::{
  io::{stdout, Write},
  thread, time,
};
use log::{info};

/// Entry point for the command-line interface. Pre-parse command line args
/// by calling StartArgs methods, or just use default constructed StartArgs.
pub fn start_emulator(args: &mut ErlStartArgs) {
  if cfg!(feature = "r20") {
    info!("Erlang Runtime (compat OTP 20)");
  }
  if cfg!(feature = "r21") {
    info!("Erlang Runtime (compat OTP 21)");
  }
  if cfg!(feature = "r22") {
    info!("Erlang Runtime (compat OTP 22)");
  }
  if cfg!(feature = "r25") {
    info!("Erlang Runtime (compat OTP 25)");
  }
  if cfg!(feature = "r27") {
    info!("Erlang Runtime (compat OTP 27)");
  }

  let mut beam_vm = VM::new(args);

  let mfargs = ModFunArgs::with_args_list(
    atom::from_str("test2"),
    atom::from_str("test"),
    Term::nil(),
  );
  //  let mfargs = ModFunArgs::with_args_list(
  //    atom::from_str("init"),
  //    atom::from_str("boot"),
  //    args.get_command_line_list().unwrap(),
  //  );
  let _rootp = beam_vm
    .create_process(Term::nil(), &mfargs, &SpawnOptions::default())
    .unwrap();

  info!("Process created. Entering main loop...");
  while beam_vm.tick().unwrap() {
    thread::sleep(time::Duration::from_millis(0));
  }
  stdout().flush().unwrap();
}
