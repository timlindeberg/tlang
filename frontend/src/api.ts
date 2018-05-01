const API_URL = 'localhost:9000';

export async function getDocumentation(): Promise<String[]> {
  const response = await fetch(`http://${API_URL}/documentation`);
  if (!response.ok) {
    throw new Error('Could not fetch documentation');
  }

  const json: any  = await response.json();
  return json.markdown.map((_: any) => _.content);
}

export async function connectToCompilationService(): Promise<WebSocket> {
  return new Promise<WebSocket>((resolve, reject) => {
    const socket = new WebSocket(`ws://${API_URL}/compilationWs`);
    socket.onopen = () => resolve(socket);
    socket.onerror = err =>
      reject(err);
  });
}
