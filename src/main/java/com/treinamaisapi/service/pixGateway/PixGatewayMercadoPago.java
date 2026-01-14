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
                .txId(UUID.randomUUID().toString())
                .qrCodeBase64("FAKE_QR_CODE")
                .copiaCola("000201FAKEPIX")
                .expiracao(LocalDateTime.now().plusMinutes(30))
                .build();
    }

    @Override
    public void cancelarCobranca(String txId) {
        // MVP
    }
}
