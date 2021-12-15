# ocaml-jvm

## now
```
 ❯ dune exec ocaml_jvm java_src/HelloWorld.class
magic : 0xcafebabe
minor_version : 0x0
major_version : 0x34
constant_pool_count : 0x1d
constant_pool : [
  methodref 15 6;
  fieldref 17 16;
  string 18;
  methodref 20 19;
  class 21;
  class 22;
  utf8 6 <init>;
  utf8 3 ()V;
  utf8 4 Code;
  utf8 15 LineNumberTable;
  utf8 4 main;
  utf8 22 ([Ljava/lang/String;)V;
  utf8 10 SourceFile;
  utf8 15 HelloWorld.java;
  name_and_type 8 7;
  class 23;
  name_and_type 25 24;
  utf8 12 Hello,world.;
  class 26;
  name_and_type 28 27;
  utf8 10 HelloWorld;
  utf8 16 java/lang/Object;
  utf8 16 java/lang/System;
  utf8 3 out;
  utf8 21 Ljava/io/PrintStream;;
  utf8 19 java/io/PrintStream;
  utf8 7 println;
  utf8 21 (Ljava/lang/String;)V;
]
access_flags : 0x21
this_class : 0x5
super_class : 0x6
interfaces_count : 0
interfaces : [
]
fields_count : 0
fields : [
]
methods_count : 2
methods : [
  {
    access_flags: 1;
    name_index: <init>;
    descriptor_index: ()V;
    attributes: [
      Code : {
        max_stack: 1;
        max_locals: 1;
        code: [
          42;
          183;
          0;
          1;
          177;
        ];
        exception_table: [
        ];
        attributes: [
          LineNumberTable : [
            { start_pc: 0; line_number: 1 };
          ];
        ];
      };
    ];
  };
  {
    access_flags: 9;
    name_index: main;
    descriptor_index: ([Ljava/lang/String;)V;
    attributes: [
      Code : {
        max_stack: 2;
        max_locals: 1;
        code: [
          178;
          0;
          2;
          18;
          3;
          182;
          0;
          4;
          177;
        ];
        exception_table: [
        ];
        attributes: [
          LineNumberTable : [
            { start_pc: 0; line_number: 3 };
            { start_pc: 8; line_number: 4 };
          ];
        ];
      };
    ];
  };
]
attributes_count : 1
attributes : [
  SourceFile : { 14 };
]

 ❯ xxd java_src/HelloWorld.class
00000000: cafe babe 0000 0034 001d 0a00 0600 0f09  .......4........
00000010: 0010 0011 0800 120a 0013 0014 0700 1507  ................
00000020: 0016 0100 063c 696e 6974 3e01 0003 2829  .....<init>...()
00000030: 5601 0004 436f 6465 0100 0f4c 696e 654e  V...Code...LineN
00000040: 756d 6265 7254 6162 6c65 0100 046d 6169  umberTable...mai
00000050: 6e01 0016 285b 4c6a 6176 612f 6c61 6e67  n...([Ljava/lang
00000060: 2f53 7472 696e 673b 2956 0100 0a53 6f75  /String;)V...Sou
00000070: 7263 6546 696c 6501 000f 4865 6c6c 6f57  rceFile...HelloW
00000080: 6f72 6c64 2e6a 6176 610c 0007 0008 0700  orld.java.......
00000090: 170c 0018 0019 0100 0c48 656c 6c6f 2c77  .........Hello,w
000000a0: 6f72 6c64 2e07 001a 0c00 1b00 1c01 000a  orld............
000000b0: 4865 6c6c 6f57 6f72 6c64 0100 106a 6176  HelloWorld...jav
000000c0: 612f 6c61 6e67 2f4f 626a 6563 7401 0010  a/lang/Object...
000000d0: 6a61 7661 2f6c 616e 672f 5379 7374 656d  java/lang/System
000000e0: 0100 036f 7574 0100 154c 6a61 7661 2f69  ...out...Ljava/i
000000f0: 6f2f 5072 696e 7453 7472 6561 6d3b 0100  o/PrintStream;..
00000100: 136a 6176 612f 696f 2f50 7269 6e74 5374  .java/io/PrintSt
00000110: 7265 616d 0100 0770 7269 6e74 6c6e 0100  ream...println..
00000120: 1528 4c6a 6176 612f 6c61 6e67 2f53 7472  .(Ljava/lang/Str
00000130: 696e 673b 2956 0021 0005 0006 0000 0000  ing;)V..........
00000140: 0002 0001 0007 0008 0001 0009 0000 001d  ................
00000150: 0001 0001 0000 0005 2ab7 0001 b100 0000  ........*.......
00000160: 0100 0a00 0000 0600 0100 0000 0100 0900  ................
00000170: 0b00 0c00 0100 0900 0000 2500 0200 0100  ..........%.....
00000180: 0000 09b2 0002 1203 b600 04b1 0000 0001  ................
00000190: 000a 0000 000a 0002 0000 0003 0008 0004  ................
000001a0: 0001 000d 0000 0002 000e                 ..........
```
