package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.request.QuestaoUpdateRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Questao", description = "Usuarios do sistema")
public interface QuestaoControllerSwagger {


    @PostMapping("/batch")
    ResponseEntity<List<QuestaoResponse>> criarLote(@RequestBody List<QuestaoRequest> requests);

    @GetMapping("/filtro")
    ResponseEntity<List<QuestaoResponse>> listarPorFiltro(
            @RequestParam(required = false) Long temaId,
            @RequestParam(required = false) Long capituloId,
            @RequestParam(required = false) Long subcapituloId,
            @RequestParam(required = false) String banca,
            @RequestParam(required = false) String nivel
    );

    @DeleteMapping("/{id}")
    ResponseEntity<Void> deletar(@PathVariable Long id);

    @PatchMapping("/{id}")
    ResponseEntity<QuestaoResponse> atualizarParcial(
            @PathVariable Long id,
            @RequestBody QuestaoUpdateRequest request
    );
}
