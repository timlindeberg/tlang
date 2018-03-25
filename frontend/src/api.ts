const API_URL = 'http://localhost:9000';

export async function getDocumentation(): Promise<String[]> {
  const response = await fetch(`${API_URL}/documentation`);
  if (!response.ok) {
    throw new Error('Could not fetch documentation');
  }

  const json: any  = await response.json();
  return json.markdown.map((_: any) => _.content);
}
