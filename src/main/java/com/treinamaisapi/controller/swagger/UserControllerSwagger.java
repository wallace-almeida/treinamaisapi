package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.avatar.resquest.AtualizarAvatarRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import com.treinamaisapi.common.dto.usuario.progress.ProgressoUsuarioResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
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


}
