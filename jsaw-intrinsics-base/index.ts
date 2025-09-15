
export const fast_add: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_add'] ?? ((a: any, b: any) => a + b);
export const fast_and: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_and'] ?? ((a: any, b: any) => a & b);
export const fast_or: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_or'] ?? ((a: any, b: any) => a | b);
export const fast_eq: (a: number,b: number) => boolean = (globalThis as any)['~Natives_fast_eq'] ?? ((a: any, b: any) => a === b);
export const fast_sub: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_sub'] ?? ((a: any, b: any) => a - b);
export const fast_shl: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_shl'] ?? ((a: any, b: any) => a << b);
export const fast_mul: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_mul'] ?? ((a: any, b: any) => a * b);
export const fast_imul: (a: number,b: number) => number = (globalThis as any)['~Natives_fast_imul'] ?? ((a: any, b: any) => Math.imul(a,b));
export const assert_string: (a: string) => string =  (globalThis as any)['~Natives_assert_string'] ?? ((a: string) => a)
export const assert_number: (a: number) => number = (globalThis as any)['~Natives_assert_number'] ??( (a: number) => a)
export const assert_static_fn: (a: Function) => Function = (globalThis as any)['~Natives_assert_static_fn'] ?? ((a: Function) => a)
export const comptime_string: (a: string) => string =  (globalThis as any)['~Natives_comptime_string'] ?? ((a: string) => a)
export const comptime_number: (a: number) => number = (globalThis as any)['~Natives_comptime_number'] ??( (a: number) => a)
export const comptime_static_fn: (a: Function) => Function = (globalThis as any)['~Natives_comptime_static_fn'] ?? ((a: Function) => a)
export const inlineme: () => void = (globalThis as any)['~Natives_inlineme'] ?? (()=>{})
export const inlineme_n: (n: number) => void = (globalThis as any)['~Natives_inlineme_n'] ?? ((n: number)=>{})
    