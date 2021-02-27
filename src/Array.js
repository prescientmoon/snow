exports.insertManyBeforeImpl = (eq) => (toInsert) => (before) => (array) => {
  const index = array.findIndex(eq(before));
  const copy = [...array];

  copy.splice(index, 0, ...toInsert);

  return copy;
};
