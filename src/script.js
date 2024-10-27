function addEmail(){
    let inputValue = document.getElementById("email").value;
    let url = "/addEmail/" + inputValue;
    fetch(url,{
        method: 'POST'
    }).then(() => location.reload())
}

function deleteEmail(){
    let inputValue = document.getElementById("email").value;
    let url = "/deleteEmail/" + inputValue;
    fetch(url,{
        method: 'DELETE'
    }).then(() => location.reload())
}

function sendAllMails(){
    let url = "sendMailToAll"
    fetch(url, {
        method: 'GET'
    })
}