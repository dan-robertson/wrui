
use range::{self, RangeIndex};

int_range_index! {
    #[derive(Deserialize, Serialize)]
    #[doc = "An index that refers to a byte offset in a text run. This could \
             point to the middle of a glyph."]
    #[derive(HeapSizeOf)]
    struct ByteIndex(isize)
}

