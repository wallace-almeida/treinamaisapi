package com.treinamaisapi.controller.usuario;

import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.controller.swagger.UserControllerSwagger;

import com.treinamaisapi.service.usuario.UserService;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping(path = "/api/usuario", produces = MediaType.APPLICATION_JSON_VALUE)
public class UserController implements UserControllerSwagger {

    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }


    @PostMapping("/create")
    @Override
    public ResponseEntity<Void> createUser(@RequestBody UsuarioRequest request) {
        userService.criarUsuario(request);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

}
