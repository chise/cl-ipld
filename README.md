# CL-IPLD
A Common Lisp based implementation of IPLD.
IPLD (InterPlanetary Linked Data) is the data model layer of IPFS
(InterPlanetary File System), which is a protocol, hypermedia (like
web) and global file system based on distributed hash table and
content-addressable global namespace.



## Installation

1. Clone this repository in a ql:*local-project-directories*

```
* ql:*local-project-directories*
(#P"/home/me/quicklisp/local-projects/")
* (quit)

% cd ~/quicklisp/local-projects/

% git clone git@gitlab.nijl.ac.jp:CHISE/cl-ipld.git
```

2. Register it

```
% sbcl

* (ql:register-local-projects)
NIL
* (quit)
```

3. Load it

```
% sbcl

* (ql:quickload :cl-ipld)
```


## Usage

Currently, this package only provides CID generator.

```
(ipld-generate-cid '(("Hello" . "world")))
-> "bafyreienby2jplt24irbffyiswxk2lhnpczmszpy3d2oi7niyaqltexipe"
```
