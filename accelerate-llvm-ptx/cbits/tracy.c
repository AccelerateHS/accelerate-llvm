/*
 * Module      : Data.Array.Accelerate.LLVM.PTX.Debug.Tracy
 * Copyright   : [2017..2020] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#include <stdint.h>
#include <stdlib.h>

extern uint64_t ___tracy_emit_zone_begin_alloc( uint64_t srcloc, int active );
extern void     ___tracy_emit_zone_end( uint64_t zone );

void ___tracy_emit_kernel_begin_cb( void* data )
{
    uint64_t* payload = (uint64_t*) data;
    payload[1] = ___tracy_emit_zone_begin_alloc(payload[0], 1);
}

void ___tracy_emit_kernel_end_cb( void* data )
{
    uint64_t *payload = (uint64_t*) data;
    ___tracy_emit_zone_end(payload[1]);

    /* free(data); */
}

