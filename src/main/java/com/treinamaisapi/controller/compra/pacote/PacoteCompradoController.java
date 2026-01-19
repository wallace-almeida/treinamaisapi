package com.treinamaisapi.controller.compra.pacote;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.MpPaymentStatusResponse;
import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixWebhookRequest;
import com.treinamaisapi.common.dto.compra.pix.response.CriarCompraPixResponse;
import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.controller.swagger.PacoteCompradoControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import com.treinamaisapi.service.pixGateway.PixGatewayMercadoPago;
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

    private final PixGatewayMercadoPago pixGateway;






    @PostMapping("/desativar/{id}")
    @Override
    public ResponseEntity<Void> desativarExpirado(@PathVariable Long id) {
        pacoteCompradoService.desativarPacoteExpirado(id);
        return ResponseEntity.ok().build();
    }

    // pix
    @PostMapping("/{pacoteId}/pix")
    @Override
    public CriarCompraPixResponse criarPix(
            @PathVariable Long pacoteId,
            @AuthenticationPrincipal Usuario usuario
    ) {
        return pacoteCompradoService.criarCompraPix(usuario.getId(), pacoteId);
    }

    @GetMapping("/compras/{compraId}")
    @Override
    public CompraResponse buscarCompra(
            @PathVariable Long compraId,
            @AuthenticationPrincipal Usuario usuario
    ) {
        return pacoteCompradoService.buscarCompra(compraId, usuario.getId());
    }

    @PostMapping("/pix")
    @Override
    public ResponseEntity<Void> webhookPix(@RequestBody PixWebhookRequest request) {
        System.out.println("📥 Webhook Mercado Pago: " + request);

        if (!"payment".equalsIgnoreCase(request.getType())) {
            return ResponseEntity.ok().build();
        }

        String paymentId = request.getData() != null ? request.getData().getId() : null;
        if (paymentId == null) {
            return ResponseEntity.badRequest().build();
        }

        MpPaymentStatusResponse payment = pixGateway.buscarPagamento(paymentId);

        if (payment != null && "approved".equalsIgnoreCase(payment.getStatus())) {
            pacoteCompradoService.confirmarPagamentoPix(String.valueOf(payment.getId()));
        }

        return ResponseEntity.ok().build();
    }







}
