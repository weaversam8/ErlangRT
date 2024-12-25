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
  // start basic:start/0
  let mfargs = ModFunArgs::with_args_list(
    atom::from_str("basic"),
    atom::from_str("start"),
    Term::nil(),
  );

  // start test2:test/0
  // let mfargs = ModFunArgs::with_args_list(
  //   atom::from_str("test2"),
  //   atom::from_str("test"),
  //   Term::nil(),
  // );
  
  // start init:boot/0
  //  let mfargs = ModFunArgs::with_args_list(
  //    atom::from_str("init"),
  //    atom::from_str("boot"),
  //    args.get_command_line_list().unwrap(),
  //  );

  start_emulator_with_mfargs(args, mfargs);
}

pub fn start_emulator_with_mfargs(args: &mut ErlStartArgs, mfargs: ModFunArgs) {
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

  let _rootp = beam_vm
    .create_process(Term::nil(), &mfargs, &SpawnOptions::default())
    .unwrap();

  info!("Process created. Entering main loop...");
  while beam_vm.tick().unwrap() {
    thread::sleep(time::Duration::from_millis(0));
  }
  stdout().flush().unwrap();
}

mod tests {
  use crate::{
    command_line_args::ErlStartArgs,
    emulator::{atom, mfa::ModFunArgs, spawn_options::SpawnOptions, vm::VM},
    term::*,
  };

  use super::*;

  fn setup() -> ErlStartArgs {
    let in_args: Vec<String> = vec!["erlexec".to_string()];
    let mut args = ErlStartArgs::new(&in_args);
    args.populate_with(in_args.iter());

    args.search_path = vec![
      "../priv/".to_string(),
    ];

    args
  }

  #[test]
  fn it_works() {
    let mut args = setup();
    let mfargs = ModFunArgs::with_args_list(
      atom::from_str("basic"),
      atom::from_str("start"),
      Term::nil(),
    );

    start_emulator_with_mfargs(&mut args, mfargs);
  }
}