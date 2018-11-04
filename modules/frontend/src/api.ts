export async function getDocumentation(): Promise<String[]> {
  const response = await fetch(getUrl('http', 'api/documentation'));
  if (!response.ok) {
    throw new Error('Could not fetch documentation');
  }

  const json: any  = await response.json();
  return json.markdown.map((_: any) => _.content);
}

export async function connectToCompilationService(): Promise<WebSocket> {
  return new Promise<WebSocket>((resolve, reject) => {
    const socket = new WebSocket(getUrl('ws', 'api/compilationWs'));
    socket.onopen = () => resolve(socket);
    socket.onerror = err =>
      reject(err);
  });
}

function getUrl(prefix: string, path: string): string {
  // We need this since we use a relative path in production but
  // use a seperate webpack dev server in dev mode
  let url = prefix;
  if (window.location.protocol === 'https:') {
    url += 's';
  }
  if (process.env.NODE_ENV === 'production') {
    return `${url}://${window.location.host}/${path}`;
  }
  return `${url}://localhost:9000/${path}`;
}
