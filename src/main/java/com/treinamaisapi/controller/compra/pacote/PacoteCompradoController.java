package com.treinamaisapi.controller.compra.pacote;

import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.controller.swagger.PacoteCompradoControllerSwagger;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/comprar", produces = MediaType.APPLICATION_JSON_VALUE)
public class PacoteCompradoController implements PacoteCompradoControllerSwagger{

    private final PacoteCompradoService pacoteCompradoService;

    @PostMapping("/pacote/{pacoteId}/usuario/{usuarioId}")
    @Override
    public CompraResponse comprarPacote(
            @PathVariable Long usuarioId,
            @PathVariable Long pacoteId) {
        return pacoteCompradoService.comprarPacote(usuarioId, pacoteId);
    }




    @PostMapping("/desativar/{id}")
    @Override
    public ResponseEntity<Void> desativarExpirado(@PathVariable Long id) {
        pacoteCompradoService.desativarPacoteExpirado(id);
        return ResponseEntity.ok().build();
    }



}
