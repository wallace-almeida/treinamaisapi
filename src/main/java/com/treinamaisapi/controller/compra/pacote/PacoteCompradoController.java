package com.treinamaisapi.controller.compra.pacote;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixWebhookRequest;
import com.treinamaisapi.common.dto.compra.pix.response.CriarCompraPixResponse;
import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.controller.swagger.PacoteCompradoControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/comprar", produces = MediaType.APPLICATION_JSON_VALUE)
public class PacoteCompradoController implements PacoteCompradoControllerSwagger{

    private final PacoteCompradoService pacoteCompradoService;

    @PostMapping("/pacote/{pacoteId}")
    @Override
    public CompraResponse comprarPacote(
            @PathVariable Long pacoteId,
            @AuthenticationPrincipal Usuario usuarioAutenticado) {

        return pacoteCompradoService.comprar(usuarioAutenticado.getId(), pacoteId);


    }




    @PostMapping("/desativar/{id}")
    @Override
    public ResponseEntity<Void> desativarExpirado(@PathVariable Long id) {
        pacoteCompradoService.desativarPacoteExpirado(id);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/pacotes/{pacoteId}/pix")
    public CriarCompraPixResponse criarPix(
            @PathVariable Long pacoteId,
            @AuthenticationPrincipal Usuario usuario
    ) {
        return pacoteCompradoService.criarCompraPix(usuario.getId(), pacoteId);
    }

    @PostMapping("/webhooks/pix")
    public ResponseEntity<Void> webhookPix(@RequestBody PixWebhookRequest request) {

        if ("approved".equalsIgnoreCase(request.getStatus())) {
            pacoteCompradoService.confirmarPagamentoPix(request.getTxId());
        }

        return ResponseEntity.ok().build();
    }




}
