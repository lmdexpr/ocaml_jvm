module Reader = Reader
module U8 = Reader.U8
module U16 = struct
  include Reader.U16
  include Uint.U16
end
module U32 = Reader.U32
