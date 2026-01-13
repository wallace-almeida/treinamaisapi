package com.treinamaisapi.controller.acessoControlle;

import com.treinamaisapi.controller.swagger.AcessoControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.compra.pacote.acessValid.AcessoPacoteService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/acesso", produces = MediaType.APPLICATION_JSON_VALUE)
public class AcessoController implements AcessoControllerSwagger {

    private final AcessoPacoteService service;

    @GetMapping("/pacote/{pacoteId}")
    @Override
    public ResponseEntity<?> temAcesso(@PathVariable Long pacoteId,
                                       @AuthenticationPrincipal Usuario usuario) {

        boolean acesso = service.usuarioTemAcesso(usuario.getId(), pacoteId);

        return ResponseEntity.ok(Map.of("temAcesso", acesso));
    }

}
