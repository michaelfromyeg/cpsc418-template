# CPSC 418 Template

For 2023W2.

To get started, run `bash scripts/install.sh`. Should work on macOS or Linux.

On Windows? Get [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) (and Windows Terminal and Cascadia Code) and don't look back.

## About

This is mostly Erlang stuff; for CUDA, you'll need to work on the remote servers (unless you happen to have some A100s kicking around!).

## Tips

Other tips.

Add this to your `~/.ssh/config`.

```plaintext
Host remote lulu annacis bowen thetis paxos
        HostName %h.students.cs.ubc.ca
        User <CWL>
        IdentityFile ~/.ssh/id_ubc
```

This assumes you've run `ssh-keygen` on your device, and copied the public key over to the remote host. Google "GitHub SSH key setup" if you get stuck, GitHub's guide is quite good.

From this box, you can run `lin01` through `lin25` to connect to CUDA ready PCs.
