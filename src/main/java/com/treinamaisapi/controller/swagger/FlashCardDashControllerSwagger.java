package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.flashcard.cartao.CartaoRequest;
import com.treinamaisapi.common.dto.flashcard.cartao.CartaoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.RevisaoPendenteResponse;
import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardsDashboardResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Flash Card dashboard", description = " Dashboard Cartoes do Usuarios ")
public interface FlashCardDashControllerSwagger {


    @GetMapping("/dashboard")
    ResponseEntity<FlashcardsDashboardResponse> dashboard(
            @AuthenticationPrincipal Usuario usuario);
}
