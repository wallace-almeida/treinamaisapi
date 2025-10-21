package com.treinamaisapi.controller.questao;

import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import com.treinamaisapi.controller.swagger.QuestaoControllerSwagger;
import com.treinamaisapi.service.questao.QuestaoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/questoes", produces = MediaType.APPLICATION_JSON_VALUE)
public class QuestaoController implements QuestaoControllerSwagger {

    private final QuestaoService questaoService;

    @PostMapping("/batch")
    @Override
    public ResponseEntity<List<QuestaoResponse>> criarLote(@RequestBody List<QuestaoRequest> requests) {
        return ResponseEntity.status(201).body(questaoService.criarLote(requests));
    }

    @GetMapping("/filtro")
    @Override
    public ResponseEntity<List<QuestaoResponse>> listarPorFiltro(
            @RequestParam(required = false) Long temaId,
            @RequestParam(required = false) Long capituloId,
            @RequestParam(required = false) Long subcapituloId,
            @RequestParam(required = false) String banca,
            @RequestParam(required = false) String nivel
    ) {
        return ResponseEntity.ok(questaoService.listarPorFiltro(temaId, capituloId, subcapituloId, banca, nivel));
    }

}
