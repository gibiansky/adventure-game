eventName = "downloadPicture1";

if (!localStorage[eventName]) {
  localStorage[eventName] = true;
  console.log("Hello! I am: " + eventName);
}
