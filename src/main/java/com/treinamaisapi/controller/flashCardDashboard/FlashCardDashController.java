package com.treinamaisapi.controller.flashCardDashboard;

import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardsDashboardResponse;
import com.treinamaisapi.controller.swagger.FlashCardDashControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.flashcarddash.FlashCardDashBoardService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/flashCard", produces = MediaType.APPLICATION_JSON_VALUE)
public class FlashCardDashController implements FlashCardDashControllerSwagger {

    private final FlashCardDashBoardService flashCardDashBoardService;

    @GetMapping("/dashboard")
    @Override
    public ResponseEntity<FlashcardsDashboardResponse> dashboard(
            @AuthenticationPrincipal Usuario usuario) {
        if (usuario == null) {
            return ResponseEntity.status(401).build();
        }
        return ResponseEntity.ok(
                flashCardDashBoardService.dashboard(usuario.getId())
        );
    }




}
