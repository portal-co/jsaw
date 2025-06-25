
export const fast_add = (globalThis as any)['~Natives_fast_add'] ?? ((a: any, b: any) => a + b);
export const fast_and = (globalThis as any)['~Natives_fast_and'] ?? ((a: any, b: any) => a & b);
export const fast_or = (globalThis as any)['~Natives_fast_or'] ?? ((a: any, b: any) => a | b);
export const fast_eq = (globalThis as any)['~Natives_fast_eq'] ?? ((a: any, b: any) => a === b);
export const fast_sub = (globalThis as any)['~Natives_fast_sub'] ?? ((a: any, b: any) => a - b);
export const fast_shl = (globalThis as any)['~Natives_fast_shl'] ?? ((a: any, b: any) => a << b);
export const fast_mul = (globalThis as any)['~Natives_fast_mul'] ?? ((a: any, b: any) => a * b);
export const fast_imul = (globalThis as any)['~Natives_fast_imul'] ?? ((a: any, b: any) => Math.imul(a,b));
export const assert_string =  (globalThis as any)['~Natives_assert_string'] ?? ((a: string) => a)
export const assert_number = (globalThis as any)['~Natives_assert_number'] ??( (a: number) => a)
export const assert_static_fn = (globalThis as any)['~Natives_assert_static_fn'] ?? ((a: Function) => a)
export const comptime_string =  (globalThis as any)['~Natives_comptime_string'] ?? ((a: string) => a)
export const comptime_number = (globalThis as any)['~Natives_comptime_number'] ??( (a: number) => a)
export const comptime_static_fn = (globalThis as any)['~Natives_comptime_static_fn'] ?? ((a: Function) => a)
    