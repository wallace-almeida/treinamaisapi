package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

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
}
