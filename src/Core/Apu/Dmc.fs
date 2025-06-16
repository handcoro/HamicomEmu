namespace HamicomEmu.Apu

module Dmc =

  open HamicomEmu.Apu.Types

  let needsSampleRead dmc =
    dmc.buffer.IsNone && dmc.bytesRemaining > 0uy

  let applySampleRead value dmc =
    let nextAddr =
      if dmc.currentAddress = 0xFFFFus then 0x8000us
      else dmc.currentAddress + 1us

    let nextRemaining = dmc.bytesRemaining - 1uy

    { dmc with
        buffer = Some value
        currentAddress = nextAddr
        bytesRemaining = nextRemaining }

  let tick dmc : DmcState * DmcReadRequest option =
    if needsSampleRead dmc then
      let req = {
        addr = dmc.currentAddress
        onRead = fun value -> applySampleRead value dmc
      }
      dmc, Some req
    else
      dmc, None
