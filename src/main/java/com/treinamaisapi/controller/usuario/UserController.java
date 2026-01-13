package com.treinamaisapi.controller.usuario;

import com.treinamaisapi.common.dto.avatar.resquest.AtualizarAvatarRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.perfil.AtualizarPerfilRequest;
import com.treinamaisapi.common.dto.usuario.perfil.UsuarioPerfilResponse;
import com.treinamaisapi.common.dto.usuario.progress.ProgressoUsuarioResponse;
import com.treinamaisapi.controller.swagger.UserControllerSwagger;


import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.usuario.UserService;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import org.springframework.http.ResponseEntity;

import org.springframework.security.core.annotation.AuthenticationPrincipal;
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

    @GetMapping("/progresso/{usuarioId}")
    @Override
    public ResponseEntity<ProgressoUsuarioResponse> obterProgresso(
            @PathVariable Long usuarioId) {

        return ResponseEntity.ok(
                userService.obterProgresso(usuarioId)
        );
    }


    @PutMapping("/{usuarioId}/avatar")
    @Override
    public ResponseEntity<Void> atualizarAvatar(
            @PathVariable Long usuarioId,
            @RequestBody AtualizarAvatarRequest request
    ) {
        userService.atualizarAvatar(usuarioId, request.avatarNome());
        return ResponseEntity.noContent().build();
    }

    @PutMapping("/atualizar/perfil")
    @Override
    public ResponseEntity<UsuarioPerfilResponse> atualizarPerfil(
            @AuthenticationPrincipal Usuario usuarioAutenticado,
            @RequestBody @Valid AtualizarPerfilRequest request
    ) {
        return ResponseEntity.ok(
                userService.atualizarPerfil(usuarioAutenticado.getId(), request)
        );
    }

}
