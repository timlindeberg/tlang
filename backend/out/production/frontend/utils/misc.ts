export function findFiles(ctx: any): string[] {
  const keys = ctx.keys();
  return keys.map(ctx);
}

export function findFilesWithNames(ctx: any): string[] {
  const keys = ctx.keys();
  const values = keys.map(ctx);
  return keys.reduce((o: any, k: string, i: number) => {
    const key = k.replace(/^\.\//g, '').replace(/\..+$/g, '');
    o[key] = values[i];
    return o;
  },                 {});

}

export function camelCaseToTitle(text: string): string {
  const result = text.replace(/([A-Z])/g, ' $1');
  return result.charAt(0).toUpperCase() + result.slice(1);
}
