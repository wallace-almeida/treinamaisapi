package com.treinamaisapi.service.pixGateway;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
public class PixGatewayMercadoPago implements PixGateway {

    @Override
    public PixCobrancaResponse criarCobranca(
            Long compraId,
            BigDecimal valor
    ) {
        return PixCobrancaResponse.builder()
                .txId("MP_" + UUID.randomUUID())
                .qrCodeBase64("BASE64_QR")
                .copiaCola("000201PIX...")
                .expiracao(LocalDateTime.now().plusMinutes(30))
                .build();
    }

    @Override
    public void cancelarCobranca(String txId) {
        // MVP
    }
}
