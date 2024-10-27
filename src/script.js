function addEmail(){
    let inputValue = document.getElementById("email").value;
    let url = "/addEmail/" + inputValue;
    fetch(url,{
        method: 'POST'
    })
}

function deleteEmail(){
    let inputValue = document.getElementById("email").value;
    let url = "/deleteEmail/" + inputValue;
    fetch(url,{
        method: 'DELETE'
    })
}