window.addEventListener('beforeunload', (event) => {
  event.returnValue = `Weet je zeker dat je de widget kwekerij wilt sluiten?`;
});
