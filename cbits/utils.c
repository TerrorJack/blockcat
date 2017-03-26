#include <utils.h>

size_t BinaryenLiteralSize(void) { return sizeof(struct BinaryenLiteral); }

void BinaryenLiteralRefInt32(BinaryenLiteralRef ref, int32_t x) {
  *ref = BinaryenLiteralInt32(x);
}

void BinaryenLiteralRefInt64(BinaryenLiteralRef ref, int64_t x) {
  *ref = BinaryenLiteralInt64(x);
}

void BinaryenLiteralRefFloat32(BinaryenLiteralRef ref, float x) {
  *ref = BinaryenLiteralFloat32(x);
}

void BinaryenLiteralRefFloat64(BinaryenLiteralRef ref, double x) {
  *ref = BinaryenLiteralFloat64(x);
}

void BinaryenLiteralRefFloat32Bits(BinaryenLiteralRef ref, int32_t x) {
  *ref = BinaryenLiteralFloat32Bits(x);
}

void BinaryenLiteralRefFloat64Bits(BinaryenLiteralRef ref, int64_t x) {
  *ref = BinaryenLiteralFloat64Bits(x);
}

BinaryenExpressionRef BinaryenConstRef(BinaryenModuleRef module,
                                       BinaryenLiteralRef value) {
  return BinaryenConst(module, *value);
}
