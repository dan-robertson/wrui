// we want to avoid bringing in the whole net setack from servo when
// we don't want to load fonts from the web. We can't just rely on LTO
// as the LTO can't tell that a thread never gets sent any messages.
// Therefore we implement the API for the network thread but just
// panic whenever we are called.


use net_traits::{CoreResourceThread};
use ipc_channel::ipc;
use std::thread;

pub fn new_fake_resource_thread() -> CoreResourceThread {
    let (send, recv) = ipc::channel().unwrap();
    thread::Builder::new().name("FakeResourceManager".to_owned()).spawn(move || {
        loop {
            match recv.recv() {
                Err(e) => (), // don't really care
                Ok(x) => {
                    // perhaps we could emulate dns errors or offline errors but what's the point
                    panic!("Not really a resource manager");
                }
            }
        }
    });
    send
}
