/* o/~ readings in macrology /
 *     never meant that much to me /
 *     the language I thought was just so /
 *     she tells me, we ain't compatible
 *
 * Copyright 2015 AdGear Technologies Inc.
 */

#pragma once

#define CONCAT_HELPER(x,y) x##y
#define CONCAT(x,y) CONCAT_HELPER(x,y)
#define GENSYM(x) CONCAT(x, __COUNTER__)
#define UNUSED GENSYM(_) __attribute((unused))
