module Reader = Reader

module U8 = struct
  include Reader.U8
  include Uint.U8
end

module U16 = struct
  include Reader.U16
  include Uint.U16
end

module U32 = struct
  include Reader.U32
  include Uint.U32
end
