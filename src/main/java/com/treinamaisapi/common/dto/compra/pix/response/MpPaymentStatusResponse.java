package com.treinamaisapi.common.dto.compra.pix.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MpPaymentStatusResponse {

    private Long id;
    private String status;          // ex: "pending", "approved"
    private String status_detail;   // ex: "pending_waiting_transfer", "accredited"
    private String description;
    private String external_reference;

    private TransactionDetails transaction_details;
    private PointOfInteraction point_of_interaction;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class TransactionDetails {
        private Double net_received_amount;
        private Double total_paid_amount;
        private Double overpaid_amount;
        private Double installment_amount;
        private String financial_institution;
        private String payment_method_reference_id;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PointOfInteraction {
        private String type; // "PIX"
        private TransactionData transaction_data;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class TransactionData {
        private String qr_code;
        private String qr_code_base64;
        private String ticket_url;
    }
}