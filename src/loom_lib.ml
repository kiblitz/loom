module Weave = Weave

include struct
  open Weave
  module Time = Time
  module Io = Io
  module Udp = Udp
end
