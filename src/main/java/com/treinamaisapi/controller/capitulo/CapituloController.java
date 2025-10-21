package com.treinamaisapi.controller.capitulo;

import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.controller.swagger.CapituloControllerSwagger;
import com.treinamaisapi.service.capitulo.CapituloService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/capitulos", produces = MediaType.APPLICATION_JSON_VALUE)
public class CapituloController implements CapituloControllerSwagger {

    private final CapituloService capituloService;


    @PostMapping("/create")
    @Override
    public ResponseEntity<CapituloResponse> criar(@RequestBody CapituloRequest request) {
        return ResponseEntity.status(201).body(capituloService.criar(request));
    }

    @GetMapping("/tema/{temaId}")
    @Override
    public ResponseEntity<List<CapituloResponse>> listarPorTema(@PathVariable Long temaId) {
        return ResponseEntity.ok(capituloService.listarPorTema(temaId));
    }

}
