package com.treinamaisapi.controller.desempenho;

import com.treinamaisapi.common.dto.desempenho.DesempenhoUsuarioResponse;
import com.treinamaisapi.controller.swagger.DezempenhoControllerSwagger;
import com.treinamaisapi.service.autenticacao.AuthenticationService;
import com.treinamaisapi.service.desempenho.DesempenhoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/desempenho", produces = MediaType.APPLICATION_JSON_VALUE)
public class DesempenhoController implements DezempenhoControllerSwagger {

    private final DesempenhoService desempenhoService;
    private final AuthenticationService usuarioAutenticadoService;

    @GetMapping("/{usuarioId}")
    @Override
    public ResponseEntity<DesempenhoUsuarioResponse> obterDesempenho(
            @PathVariable Long usuarioId
    ) {
        return ResponseEntity.ok(
                desempenhoService.obterDesempenho(usuarioId)
        );
    }



}
