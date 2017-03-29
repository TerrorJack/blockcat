#ifndef utils_h
#define utils_h

#include <binaryen-c.h>

#ifdef __cplusplus
extern "C" {
#endif

size_t BinaryenLiteralSize(void);
size_t BinaryenLiteralAlign(void);

typedef struct BinaryenLiteral* BinaryenLiteralRef;

void BinaryenLiteralRefInt32(BinaryenLiteralRef ref, int32_t x);
void BinaryenLiteralRefInt64(BinaryenLiteralRef ref, int64_t x);
void BinaryenLiteralRefFloat32(BinaryenLiteralRef ref, float x);
void BinaryenLiteralRefFloat64(BinaryenLiteralRef ref, double x);
void BinaryenLiteralRefFloat32Bits(BinaryenLiteralRef ref, int32_t x);
void BinaryenLiteralRefFloat64Bits(BinaryenLiteralRef ref, int64_t x);

BinaryenExpressionRef BinaryenConstRef(BinaryenModuleRef module,
                                       BinaryenLiteralRef value);

#ifdef __cplusplus
}
#endif

#endif
