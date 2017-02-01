The load-encrypted function will load a gpg2 encrypted lisp source
file.  Use this function to retrieve encrypted credentials from disk.
In this example we just set the default ssh username and password,
like so:

    (setq surrender:*default-ssh-username* "green")
    (setq surrender:*default-ssh-password* "s3kr1t")

...and the encrypt the file with gpg2, like so:

    gpg2 -r green@redhat.com --use-agent -a --encrypt secret.lisp

See the simple.lisp file for example use.

More interestingly, you could store keytab info in an encrypted source
file, which you would then use to retrieve credentials from a
centralized secrets management solution, like FreeIPA vault.
