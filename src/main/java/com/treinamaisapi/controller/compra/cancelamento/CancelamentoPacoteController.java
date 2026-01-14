package com.treinamaisapi.controller.compra.cancelamento;

import com.treinamaisapi.common.dto.cancelamentoCompra.CancelamentoCompraRequest;
import com.treinamaisapi.common.dto.cancelamentoCompra.CancelamentoCompraResponse;
import com.treinamaisapi.controller.swagger.CancelamentoPacoteControllerSwagger;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.compra.compraEfetiv.CompraService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/pacotes-comprados", produces = MediaType.APPLICATION_JSON_VALUE)
public class CancelamentoPacoteController implements CancelamentoPacoteControllerSwagger {

    private final CompraService service;

    @PostMapping("/{id}/cancelar")
    @Override
    public ResponseEntity<CancelamentoCompraResponse> cancelar(
            @PathVariable Long id,
            @RequestBody(required = false) CancelamentoCompraRequest request,
            @AuthenticationPrincipal Usuario usuario
    ) {

        PacoteComprado compra = service.cancelar(
                id,
                usuario,
                request != null ? request.getMotivo() : null
        );

        return ResponseEntity.ok(
                CancelamentoCompraResponse.builder()
                        .id(compra.getId())
                        .status(compra.getStatus())
                        .dataCancelamento(compra.getDataCancelamento())
                        .mensagem("Compra cancelada com sucesso")
                        .build()
        );
    }




}
