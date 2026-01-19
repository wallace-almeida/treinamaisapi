package com.treinamaisapi.common.dto.compra.pix.response;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class CriarCompraPixResponse {

    private Long compraId;
    private StatusCompra status;

    private String qrCodeBase64;
    private String qrCodeCopiaCola;

    private LocalDateTime expiracaoPix;

    private String ticketUrl;
}
