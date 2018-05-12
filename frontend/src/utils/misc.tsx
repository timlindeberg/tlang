import * as React from 'react';
import * as ReactDOM from 'react-dom';

export async function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

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

export function scrollTo(el: any): void {
  // inline: 'nearest' fixes an issue of the window moving horizontally when scrolling.
  el.scrollIntoView({ behavior: 'smooth', block: 'start', inline: 'nearest' });
}

export function widthOfRenderedText(text: string, className: string): number {
  const div = document.createElement('div');
  div.setAttribute('class', className);
  div.innerHTML = text;
  document.body.appendChild(div);
  const width = div.getBoundingClientRect().width;
  div.parentNode!.removeChild(div);
  return width;
}

export function asDOM(jsx: JSX.Element, wrapper: string = 'div'): HTMLElement {
  const e = document.createElement(wrapper);
  ReactDOM.render(jsx, e);
  return e;
}

export function htmlLines(s: string | string[], className: string) {
  if (typeof s === 'string') {
    return toHtmlLines((s as string).split('\n'), className);
  }

  return toHtmlLines(s as string[], className);
}

function toHtmlLines(lines: string[], className: string) {
  return lines.map((line, i) => (
    <React.Fragment key={i}>
      <span className={className}>{line}</span>
      <br/>
    </React.Fragment>
  ));
}