let usuario, contrasena; //Variables globales

var usuarios = ["jose_progra", "any_progra", "fio_progra", "sebas_progra", "clever_progra"]

function validar_ingreso() {
    usuario = document.getElementById("username").value;
    contrasena = document.getElementById("password").value;
    if (usuarios.includes(usuario) && contrasena === "estrellitas123") {
        //window.open("https://www.argar.cat");
        window.open("https://www.argar.cat","_self");
    } else {
        alertify.error('Usuario o contraseña incorrectos');
    }
}

function validar_ingreso2() {
    usuario = document.getElementById("username").value;
    contrasena = document.getElementById("password").value;
    if (usuarios.includes(usuario) && contrasena === "estrellitas123") {
        //window.open("https://www.argar.cat");
        window.open("https://www.argar.cat","_self");
    } else {
        alertify.error('Usuario o contraseña incorrectos');
    }
}