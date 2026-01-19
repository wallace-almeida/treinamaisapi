package com.treinamaisapi.common.dto.compra.pix.response;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class PixCobrancaResponse {

    private String txId;
    private String qrCodeBase64;
    private String copiaCola;
    private LocalDateTime expiracao;
    private String ticketUrl;
}

