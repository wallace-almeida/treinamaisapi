package com.treinamaisapi.controller.subCapituloControle;

import com.treinamaisapi.common.dto.questao.request.SubcapituloRequest;
import com.treinamaisapi.common.dto.questao.response.SubcapituloResponse;
import com.treinamaisapi.controller.swagger.SubCapituloControllerSwagger;
import com.treinamaisapi.service.subCapitulo.SubcapituloService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/subcapitulos", produces = MediaType.APPLICATION_JSON_VALUE)
public class SubcapituloController implements SubCapituloControllerSwagger {

    private final SubcapituloService subcapituloService;

    @PostMapping("/create")
    @Override
    public ResponseEntity<SubcapituloResponse> criar(@RequestBody SubcapituloRequest request) {
        return ResponseEntity.status(201).body(subcapituloService.criar(request));
    }

    @GetMapping("/capitulo/{capituloId}")
    @Override
    public ResponseEntity<List<SubcapituloResponse>> listarPorCapitulo(@PathVariable Long capituloId) {
        return ResponseEntity.ok(subcapituloService.listarPorCapitulo(capituloId));
    }
}
