package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@Tag(name = "User", description = "Usuarios do sistema")
public interface UserControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<Void> createUser(@RequestBody UsuarioRequest request);
}
