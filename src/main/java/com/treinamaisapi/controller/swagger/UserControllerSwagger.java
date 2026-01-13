package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.avatar.resquest.AtualizarAvatarRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import com.treinamaisapi.common.dto.usuario.perfil.AtualizarPerfilRequest;
import com.treinamaisapi.common.dto.usuario.perfil.UsuarioPerfilResponse;
import com.treinamaisapi.common.dto.usuario.progress.ProgressoUsuarioResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@Tag(name = "User", description = "Usuarios do sistema")
public interface UserControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<Void> createUser(@RequestBody UsuarioRequest request);

    @GetMapping("/progresso/{usuarioId}")
    ResponseEntity<ProgressoUsuarioResponse> obterProgresso(
            @PathVariable Long usuarioId);

    @PutMapping("/{usuarioId}/avatar")
    ResponseEntity<Void> atualizarAvatar(
            @PathVariable Long usuarioId,
            @RequestBody AtualizarAvatarRequest request
    );


    @PutMapping("/atualizar/perfil")
    ResponseEntity<UsuarioPerfilResponse> atualizarPerfil(
            @AuthenticationPrincipal Usuario usuarioAutenticado,
            @RequestBody @Valid AtualizarPerfilRequest request
    );
}
