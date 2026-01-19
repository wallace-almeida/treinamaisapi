package com.treinamaisapi.common.dto.compra.pix.marcadoPago.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.OffsetDateTime;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MpPixPaymentResponse {

    private Long id;
    private String status;

    @JsonProperty("date_of_expiration")
    private OffsetDateTime dateOfExpiration;

    @JsonProperty("point_of_interaction")
    private PointOfInteraction pointOfInteraction;

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class PointOfInteraction {

        @JsonProperty("transaction_data")
        private TransactionData transactionData;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class TransactionData {

        @JsonProperty("qr_code_base64")
        private String qrCodeBase64;

        @JsonProperty("qr_code")
        private String qrCode;

        @JsonProperty("ticket_url")
        private String ticketUrl;
    }
}
