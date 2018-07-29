import { findFilesWithNames } from 'utils/misc';

const codeExamples = findFilesWithNames(require.context('codeExamples', true, /\.t/));

export default codeExamples;
